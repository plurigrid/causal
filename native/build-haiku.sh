#!/bin/sh
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
build=${CAUSAL_BUILD_DIR:-"$here/build"}
stage="$build/package-root"

mkdir -p "$build" "$stage/apps/CausalChat/deploy"

c++ -std=c++17 -O2 -Wall -Wextra -pedantic \
  ${TLS_CXXFLAGS:-} "$here/haiku_chat.cpp" -lbe -lnetwork \
  ${TLS_LIBS:--lssl -lcrypto} -o "$build/CausalChat"
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
cp "$here/deploy/users.example.json" \
  "$stage/apps/CausalChat/deploy/users.example.json"
cp "$here/deploy/render_users.py" "$stage/apps/CausalChat/deploy/render_users.py"
cp "$here/deploy/bootstrap.py" "$stage/apps/CausalChat/deploy/bootstrap.py"
chmod 0755 "$stage/apps/CausalChat/deploy/render_users.py" \
  "$stage/apps/CausalChat/deploy/bootstrap.py"
mimeset -f "$stage/apps/CausalChat/CausalChat"

if test "${1:-}" = "--package"; then
  package create -C "$stage" -i "$here/.PackageInfo" \
    "$build/causal_chat-0.4.0-2-x86_64.hpkg"
fi

echo "built $build/CausalChat"
