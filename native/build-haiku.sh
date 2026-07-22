#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
build=${CAUSAL_BUILD_DIR:-"$here/build"}
stage="$build/package-root"
tls_cxxflags=${TLS_CXXFLAGS:-}
tls_libs=${TLS_LIBS:-}

mkdir -p "$build" "$stage/apps/CausalChat/deploy/trust"

if test -n "${OPENSSL3_DEVEL_ROOT:-}"; then
  openssl_headers="$OPENSSL3_DEVEL_ROOT/develop/headers"
  test -f "$openssl_headers/openssl/err.h"
  tls_cxxflags="$tls_cxxflags -I$openssl_headers"
  if test -z "$tls_libs"; then
    openssl_links="$build/openssl-link"
    mkdir -p "$openssl_links"
    test -f /boot/system/lib/libssl.so.3
    test -f /boot/system/lib/libcrypto.so.3
    ln -sf /boot/system/lib/libssl.so.3 "$openssl_links/libssl.so"
    ln -sf /boot/system/lib/libcrypto.so.3 "$openssl_links/libcrypto.so"
    tls_libs="-L$openssl_links -lssl -lcrypto"
  fi
fi

if test -z "$tls_libs"; then
  tls_libs="-lssl -lcrypto"
fi

c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  $tls_cxxflags "$here/haiku_chat.cpp" "$here/connection_profile.cpp" \
  -lbe -lnetwork \
  $tls_libs -o "$build/CausalChat"
c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  "$here/nats_chat.cpp" -lnetwork -o "$build/nats-chat"

cp "$build/CausalChat" "$stage/apps/CausalChat/CausalChat"
cp "$build/nats-chat" "$stage/apps/CausalChat/nats-chat"
cp "$here/README.md" "$stage/apps/CausalChat/README.md"
cp "$here/DEPLOYMENT.md" "$stage/apps/CausalChat/DEPLOYMENT.md"
cp "$here/tests/DEPLOYMENT_EVIDENCE.md" \
  "$stage/apps/CausalChat/DEPLOYMENT_EVIDENCE.md"
cp "$here/deploy/README.md" "$stage/apps/CausalChat/deploy/README.md"
cp "$here/deploy/server.conf" "$stage/apps/CausalChat/deploy/server.conf"
cp "$here/deploy/server-funnel.conf" \
  "$stage/apps/CausalChat/deploy/server-funnel.conf"
cp "$here/deploy/users.example.json" \
  "$stage/apps/CausalChat/deploy/users.example.json"
cp "$here/deploy/render_users.py" "$stage/apps/CausalChat/deploy/render_users.py"
cp "$here/deploy/bootstrap.py" "$stage/apps/CausalChat/deploy/bootstrap.py"
cp "$here/deploy/generate_roster.py" \
  "$stage/apps/CausalChat/deploy/generate_roster.py"
cp "$here/deploy/member_probe.py" \
  "$stage/apps/CausalChat/deploy/member_probe.py"
cp "$here/deploy/provision_server.py" \
  "$stage/apps/CausalChat/deploy/provision_server.py"
cp "$here/deploy/trust/causal-chat-ca-v1.crt" \
  "$stage/apps/CausalChat/deploy/trust/causal-chat-ca-v1.crt"
chmod 0755 "$stage/apps/CausalChat/deploy/render_users.py" \
  "$stage/apps/CausalChat/deploy/bootstrap.py" \
  "$stage/apps/CausalChat/deploy/generate_roster.py" \
  "$stage/apps/CausalChat/deploy/member_probe.py" \
  "$stage/apps/CausalChat/deploy/provision_server.py"
mimeset -f "$stage/apps/CausalChat/CausalChat"

if test "${1:-}" = "--package"; then
  package create -C "$stage" -i "$here/.PackageInfo" \
    "$build/causal_chat-0.4.0-5-x86_64.hpkg"
fi

echo "built $build/CausalChat"
