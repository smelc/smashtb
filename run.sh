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

# Slack is optional, and needs both tokens: the user token reads as you, the
# app-level one opens the Socket Mode connection. Neither goes into config.js:
# the server below makes every Slack call, so the page only learns whether
# Slack is on.
if [ -n "${SLACK_APP_OAUTH_TOKEN:-}" ] && [ -n "${SLACK_APP_APP_LEVEL_TOKEN:-}" ]; then
  SLACK_ON=true
else
  SLACK_ON=false
  if [ -n "${SLACK_APP_OAUTH_TOKEN:-}${SLACK_APP_APP_LEVEL_TOKEN:-}" ]; then
    echo "Only one of SLACK_APP_OAUTH_TOKEN and SLACK_APP_APP_LEVEL_TOKEN is set;" >&2
    echo "Slack needs both, so it stays off." >&2
  fi
fi

# JSON-encode the token so quotes and backslashes cannot break out of the string.
GITHUB_TOKEN="${GITHUB_TOKEN:-}" SLACK_ON="$SLACK_ON" python3 - > _site/config.js <<'TOKEN_PY'
import json, os
print("window.SMASHTB_GITHUB_TOKEN = %s;" % json.dumps(os.environ.get("GITHUB_TOKEN", "")))
print("window.SMASHTB_SLACK = %s;" % os.environ["SLACK_ON"])
TOKEN_PY
chmod 600 _site/config.js

# Bind inside python rather than probing ports from the shell first: claiming
# the port and serving on it are then a single step, so nobody else can take it
# between the check and the bind.
exec python3 - "$BASE_PORT" "$PORT_TRIES" _site <<'SERVE_PY'
import errno
import http.server
import json
import os
import signal
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.request

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
port = 0

SLACK_USER_TOKEN = os.environ.get("SLACK_APP_OAUTH_TOKEN", "")
SLACK_APP_TOKEN = os.environ.get("SLACK_APP_APP_LEVEL_TOKEN", "")
# The Web API methods the page may call with the user token. All of them only
# read, so even a request that got past the checks in do_POST could not post
# or change anything as you.
SLACK_METHODS = {
    "auth.test",
    "bots.info",
    "conversations.history",
    "conversations.info",
    "conversations.list",
    "conversations.replies",
    "users.conversations",
    "users.info",
}

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

    def do_POST(self):
        path = self.path.split("?")[0]
        if not self.from_our_page():
            self.send_error(403)
            return
        length = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(length) if length > 0 else b""
        if path == "/slack/connect":
            self.slack("apps.connections.open", SLACK_APP_TOKEN, b"")
        elif path.startswith("/slack/api/"):
            method = path[len("/slack/api/"):]
            if method in SLACK_METHODS:
                self.slack(method, SLACK_USER_TOKEN, body)
            else:
                self.send_error(404)
        else:
            self.send_error(404)

    def from_our_page(self):
        """Whether a request comes from this app's own page.

        Any web site open in the browser can send requests to localhost. The
        Host check turns away DNS rebinding. The custom header turns away the
        rest: a cross-origin page can only set it after a CORS preflight, and
        this server answers none."""
        hosts = {"localhost:%d" % port, "127.0.0.1:%d" % port}
        if self.headers.get("Host") not in hosts:
            return False
        origin = self.headers.get("Origin")
        if origin is not None and origin not in {"http://" + h for h in hosts}:
            return False
        return self.headers.get("X-Smashtb") == "1"

    def slack(self, method, token, body):
        """Forward a Web API call to Slack with [token], and Slack's answer
        back to the page."""
        if not token:
            self.send_json(200, b'{"ok": false, "error": "token_not_set"}')
            return
        req = urllib.request.Request(
            "https://slack.com/api/" + method,
            data=body,
            method="POST",
            headers={
                "Authorization": "Bearer " + token,
                "Content-Type": "application/x-www-form-urlencoded",
            },
        )
        try:
            with urllib.request.urlopen(req, timeout=20) as resp:
                self.send_json(resp.status, resp.read())
        except urllib.error.HTTPError as e:
            self.send_json(e.code, e.read())
        except (urllib.error.URLError, OSError) as e:
            msg = "could not reach slack.com: %s" % getattr(e, "reason", e)
            self.send_json(502, json.dumps({"ok": False, "error": msg}).encode())

    def send_json(self, status, data):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

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
