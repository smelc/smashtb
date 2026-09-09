#!/usr/bin/env bash
#
# Build the app and serve it on the first free port at or after $PORT (8080 by
# default), scanning 32 ports in all.
#
# GITHUB_TOKEN must be set: it is baked into _site/config.js so the page can
# authenticate against api.github.com. _site is gitignored; the token never
# leaves this machine except in the Authorization header sent to GitHub.

set -euo pipefail
cd "$(dirname "$0")"

BASE_PORT="${PORT:-8080}"
PORT_TRIES=32

if [ "${1:-}" = "--port" ]; then
  if [ -z "${2:-}" ]; then
    echo "--port needs a number" >&2
    exit 2
  fi
  BASE_PORT="$2"
fi

case "$BASE_PORT" in
  '' | *[!0-9]*)
    echo "port must be a number, got: $BASE_PORT" >&2
    exit 2
    ;;
esac

if [ -d _opam ]; then
  eval "$(opam env --switch="$PWD" --set-switch)"
fi

if [ -z "${GITHUB_TOKEN:-}" ]; then
  echo "GITHUB_TOKEN is not set." >&2
  echo "  export GITHUB_TOKEN=ghp_...   (or put it in a .env file, picked up by direnv)" >&2
  echo "The app still starts, but you will have to paste a token into the page." >&2
fi

dune build

rm -rf _site
mkdir -p _site
cp web/index.html web/style.css _site/
cp _build/default/src/main.bc.js _site/smashtb.js

# JSON-encode the token so quotes and backslashes cannot break out of the string.
GITHUB_TOKEN="${GITHUB_TOKEN:-}" python3 - > _site/config.js <<'TOKEN_PY'
import json, os
print("window.SMASHTB_GITHUB_TOKEN = %s;" % json.dumps(os.environ.get("GITHUB_TOKEN", "")))
TOKEN_PY
chmod 600 _site/config.js

# Bind inside python rather than probing ports from the shell first: claiming
# the port and serving on it are then a single step, so nobody else can take it
# between the check and the bind.
exec python3 - "$BASE_PORT" "$PORT_TRIES" _site <<'SERVE_PY'
import errno
import functools
import http.server
import sys

base, tries, directory = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=directory)
last = base + tries - 1

for port in range(base, last + 1):
    try:
        httpd = http.server.ThreadingHTTPServer(("127.0.0.1", port), handler)
    except OSError as e:
        # Already listening, or a port we are not allowed to bind. Try the next.
        if e.errno in (errno.EADDRINUSE, errno.EACCES):
            continue
        raise
    break
else:
    sys.exit("no free port between %d and %d" % (base, last))

if port != base:
    print("ports %d-%d are busy" % (base, port - 1))
print("smashtb serving on http://localhost:%d/  (Ctrl-C to stop)" % port, flush=True)
try:
    httpd.serve_forever()
except KeyboardInterrupt:
    print()
SERVE_PY
