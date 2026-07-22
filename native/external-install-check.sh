#!/bin/sh
# Privacy-minimized end-to-end receipt for an independently downloaded HPKG.
set -eu

if test "$#" -ne 2; then
  echo "usage: external-install-check.sh PACKAGE.hpkg EXPECTED_SHA256" >&2
  exit 2
fi

package_file=$1
expected_sha=$2
if test "${#expected_sha}" -ne 64; then
  echo "expected SHA-256 must be 64 lowercase hexadecimal characters" >&2
  exit 2
fi
case "$expected_sha" in
  *[!0-9a-f]*)
    echo "expected SHA-256 must be 64 lowercase hexadecimal characters" >&2
    exit 2
    ;;
esac

if test ! -f "$package_file"; then
  echo "package not found: $package_file" >&2
  exit 2
fi

actual_sha=$(sha256sum "$package_file" | awk '{print $1}')
if test "$actual_sha" != "$expected_sha"; then
  echo "package checksum mismatch" >&2
  exit 1
fi

metadata=$(package info -f '%name% %version%' "$package_file")
case "$metadata" in
  "causal_chat 0.4.0-6") ;;
  *) echo "unexpected package metadata: $metadata" >&2; exit 1 ;;
esac

signature=application/x-vnd.plurigrid-causal-chat
if hey -s "$signature" GET Title of Window 0 >/dev/null 2>&1; then
  echo "close the existing Causal Chat window before running this check" >&2
  exit 1
fi

receipt_root=$(mktemp -d "${TMPDIR:-/tmp}/causal-external-check.XXXXXX")
extract_root="$receipt_root/root"
mkdir "$extract_root"
package extract -C "$extract_root" "$package_file"

app="$extract_root/apps/CausalChat/CausalChat"
probe="$extract_root/apps/CausalChat/nats-chat"
test -x "$app"
test -x "$probe"

app_sha=$(sha256sum "$app" | awk '{print $1}')
history="$receipt_root/history.ndjson"
app_log="$receipt_root/CausalChat.log"
marker="rc6-$(date +%s)-$$"
payload="{\"id\":\"$marker\",\"sender\":\"external-check\",\"text\":\"Public RC6 installation roundtrip; no private content.\"}"
app_pid=

finish() {
  if test -n "$app_pid"; then
    hey -s "$signature" QUIT >/dev/null 2>&1 || true
    wait "$app_pid" 2>/dev/null || true
    app_pid=
  fi
}
trap finish EXIT HUP INT TERM

NATS_HOST=nonlocal.info
NATS_PORT=4222
NATS_TLS=0
NATS_JETSTREAM=0
CAUSAL_PETNAME=external-check
CAUSAL_HISTORY=1
CAUSAL_HISTORY_PATH=$history
CAUSAL_PROFILE_PATH="$receipt_root/profile"
export NATS_HOST NATS_PORT NATS_TLS NATS_JETSTREAM
export CAUSAL_PETNAME CAUSAL_HISTORY CAUSAL_HISTORY_PATH CAUSAL_PROFILE_PATH

"$app" >"$app_log" 2>&1 &
app_pid=$!
sleep 3
if ! kill -0 "$app_pid" 2>/dev/null; then
  echo "native application exited before the network check" >&2
  exit 1
fi

received=0
attempt=1
while test "$attempt" -le 5; do
  "$probe" say chat.room.lobby "$payload"
  waited=0
  while test "$waited" -lt 4; do
    if test -f "$history" && grep -Fq "\"id\":\"$marker\"" "$history"; then
      received=1
      break
    fi
    sleep 1
    waited=$((waited + 1))
  done
  test "$received" -eq 1 && break
  attempt=$((attempt + 1))
done

if test "$received" -ne 1; then
  echo "the published marker did not cross the native UI/history boundary" >&2
  exit 1
fi

marker_count=$(grep -Fc "\"id\":\"$marker\"" "$history")
if test "$marker_count" -ne 1; then
  echo "marker was not deduplicated to exactly one history record" >&2
  exit 1
fi

finish
marker_sha=$(printf '%s' "$marker" | sha256sum | awk '{print $1}')

printf '%s\n' \
  'receipt_schema=causal-haiku-external-v1' \
  'release=haiku-chat-v0.4.0-rc6' \
  "package_sha256=$actual_sha" \
  "application_sha256=$app_sha" \
  "os=$(uname -s)" \
  "os_release=$(uname -r)" \
  "machine=$(uname -m)" \
  'transport=public-plaintext-nonsensitive-check-only' \
  'roundtrip=pass' \
  "history_marker_count=$marker_count" \
  "marker_sha256=$marker_sha" \
  'local_evidence_retained=yes'
printf 'local evidence retained at %s\n' "$receipt_root" >&2
