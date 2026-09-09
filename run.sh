#!/usr/bin/env bash
# Build the app and serve it on the first free port at or after $PORT (8080 by
# default), scanning 32 ports in all.
#
# The sources are watched: dune rebuilds on every edit, and the browser reloads
# itself once the new build lands.
#
# GITHUB_TOKEN must be set: it is baked into _site/config.js so the page can
# authenticate against api.github.com. _site is gitignored; the token never
# leaves this machine except in the Authorization header sent to GitHub.
set -euo pipefail
cd "$(dirname "$0")"

BASE_PORT="${PORT:-8080}"
PORT_TRIES=32

usage() {
  cat <<'USAGE'
usage: ./run.sh [--port N]

  --port N     start looking for a free port at N (default 8080, 32 tried)
USAGE
}

while [ $# -gt 0 ]; do
  case "$1" in
    --port)
      shift
      if [ $# -eq 0 ]; then
        echo "--port needs a number" >&2
        exit 2
      fi
      BASE_PORT="$1"
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      echo "unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

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
  echo "The app still starts, but every call to GitHub will be refused." >&2
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
import http.server
import os
import signal
import subprocess
import sys
import threading
import time

base, tries, directory = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
last = base + tries - 1

# Built file -> name under _site. config.js is deliberately absent: it holds the
# token and is written once, by run.sh.
SOURCES = [
    ("_build/default/src/main.bc.js", "smashtb.js"),
    ("web/index.html", "index.html"),
    ("web/style.css", "style.css"),
]

version = 0
url = ""

RELOAD_JS = b"""<script>
(function () {
  var seen = null;
  var es = new EventSource("/__reload");
  es.onmessage = function (e) {
    if (seen === null) { seen = e.data; return; }
    if (e.data !== seen) { location.reload(); }
  };
})();
</script>
"""


def stamp():
    out = []
    for src, _ in SOURCES:
        try:
            st = os.stat(src)
            out.append((st.st_mtime_ns, st.st_size))
        except OSError:
            out.append(None)
    return tuple(out)


def sync():
    """Copy anything that changed into _site. Returns True if something did."""
    changed = False
    for src, name in SOURCES:
        dst = os.path.join(directory, name)
        try:
            with open(src, "rb") as f:
                data = f.read()
        except OSError:
            continue
        try:
            with open(dst, "rb") as f:
                if f.read() == data:
                    continue
        except OSError:
            pass
        # dune leaves its output read-only, so write a new file and rename over
        # the old one rather than opening the destination for writing.
        tmp = dst + ".new"
        with open(tmp, "wb") as f:
            f.write(data)
        os.replace(tmp, dst)
        changed = True
    return changed


def watcher():
    global version
    seen = stamp()
    settling = None
    while True:
        time.sleep(0.25)
        now = stamp()
        if now == seen:
            continue
        # Only copy once the files have stopped changing, so a rebuild that is
        # still running is never served half written.
        if now != settling:
            settling = now
            continue
        seen, settling = now, None
        if sync():
            version += 1
            # Repeat the address, so it stays in view however much dune prints.
            print("rebuilt, reloading %s" % url, flush=True)


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=directory, **kwargs)

    def log_message(self, fmt, *args):
        if "__reload" not in self.path:
            super().log_message(fmt, *args)

    def end_headers(self):
        # Without this the browser serves the old bundle from cache after a
        # reload, which defeats the whole exercise.
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def do_GET(self):
        path = self.path.split("?")[0]
        if path == "/__reload":
            self.reload_events()
        elif path in ("/", "/index.html"):
            self.index_with_reload()
        else:
            super().do_GET()

    def index_with_reload(self):
        try:
            with open(os.path.join(directory, "index.html"), "rb") as f:
                page = f.read()
        except OSError:
            self.send_error(404)
            return
        page = page.replace(b"</body>", RELOAD_JS + b"</body>", 1)
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(page)))
        self.end_headers()
        self.wfile.write(page)

    def reload_events(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Connection", "keep-alive")
        self.end_headers()
        sent, ticks = None, 0
        try:
            while True:
                if version != sent:
                    sent = version
                    self.wfile.write(b"data: %d\n\n" % sent)
                    self.wfile.flush()
                ticks += 1
                if ticks % 60 == 0:  # keep proxies and idle timeouts at bay
                    self.wfile.write(b": ping\n\n")
                    self.wfile.flush()
                time.sleep(0.25)
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass


for port in range(base, last + 1):
    try:
        httpd = http.server.ThreadingHTTPServer(("127.0.0.1", port), Handler)
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

# The rebuild loop is a child of this process rather than of run.sh, because
# the shell cannot run a trap while a foreground child is going: owning it here
# means it is always torn down with the server. Its output goes to the terminal
# so that compile errors are visible.
def dune_preexec():
    """Ask Linux to signal the rebuild loop if this server is killed outright,
    so that even a SIGKILL cannot leave it behind holding the dune lock."""
    try:
        import ctypes

        PR_SET_PDEATHSIG = 1
        ctypes.CDLL("libc.so.6", use_errno=True).prctl(PR_SET_PDEATHSIG, signal.SIGTERM)
    except Exception:
        pass  # not Linux, or no prctl: the handlers below still cover the rest


def stop(signum, frame):
    raise KeyboardInterrupt


# Python only unwinds on SIGINT, so without these a plain `kill` would stop the
# server without ever reaching the cleanup below.
signal.signal(signal.SIGTERM, stop)
signal.signal(signal.SIGHUP, stop)

url = "http://localhost:%d/" % port
print("smashtb serving on %s  (Ctrl-C to stop)" % url)
print("watching for edits; the browser reloads itself after each rebuild", flush=True)

# dune's output goes through a pipe rather than straight to the terminal. On a
# pipe it drops the progress display that redraws and would otherwise wipe the
# lines above, while still printing compile errors, which the relay below tags
# so it is clear who said what.
dune = subprocess.Popen(
    ["dune", "build", "--watch"],
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    preexec_fn=dune_preexec,
)


def relay():
    for line in dune.stdout:
        print("dune | " + line.decode("utf-8", "replace").rstrip("\n"), flush=True)
    # Reap it, so a dune that steps aside early does not linger as a zombie.
    code = dune.wait()
    if code != 0:
        print("dune | watch loop exited (%d); edits will not rebuild" % code, flush=True)


threading.Thread(target=relay, daemon=True).start()
threading.Thread(target=watcher, daemon=True).start()
try:
    httpd.serve_forever()
except KeyboardInterrupt:
    print()
finally:
    dune.terminate()
    try:
        dune.wait(timeout=5)
    except subprocess.TimeoutExpired:
        dune.kill()
SERVE_PY
