[![ocaml](https://github.com/smelc/smashtb/actions/workflows/ocaml.yml/badge.svg)](https://github.com/smelc/smashtb/actions/workflows/ocaml.yml)
[![shellcheck](https://github.com/smelc/smashtb/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/smelc/smashtb/actions/workflows/shellcheck.yml)

# smashtb

A single-page app for working through a batch of GitHub pull requests. Paste
the links, read the diffs, approve or dismiss each one.

Written in OCaml 5.5.1 and compiled to JavaScript with js_of_ocaml, using
[brr](https://erratique.ch/software/brr) for the DOM and fetch bindings.

---

<div align="center">
  <img width="709" height="718" alt="image" src="https://github.com/user-attachments/assets/49122116-1904-41b5-b3e4-a817763ea537" />
</div>

---

## Getting started

```
./setup.sh          # creates ./_opam with OCaml 5.5.1 and the dependencies
direnv allow        # so the switch loads automatically from now on
export GITHUB_TOKEN=github_pat_...
./run.sh            # builds, then serves on http://localhost:8080
```

If port 8080 is taken, `run.sh` walks up to 8111 and uses the first free port,
printing the one it settled on. `./run.sh --port 9000`, or `PORT=9000 ./run.sh`,
starts the scan somewhere else instead.

`run.sh` writes the token into `_site/config.js`, which the page reads as
`window.SMASHTB_GITHUB_TOKEN`. `_site` is gitignored and the file is created
with mode 600. That is the only way in: there is no field to paste a token
into, so a page served without `config.js` can do nothing until it is
restarted with a token.

`direnv` also reads a `.env` file if you have one, so
`echo 'export GITHUB_TOKEN=...' > .env` saves exporting it every time. That file
is gitignored too.

## Using it

Paste one pull request per line into the top box and press **Load**. These all work:

```
https://github.com/owner/repo/pull/12
https://github.com/owner/repo/pull/12/files
https://github.com/owner/repo/pull/12#issuecomment-1234
owner/repo#12
```

Each pull request becomes a card that folds shut, and inside it the list of
changed files folds too, as does every individual file. Diffs are only built
when you open a file, so a pull request touching hundreds of files still opens
instantly.

Cards are ordered by how close they are to landing, not by the order you
pasted them: the ones targeting the default branch come first, then the ones
stacked on those, and so on down the stack. Pull requests at the same depth go
in number order, smallest first, and anything whose base is neither the default
branch nor a branch on the page goes last. A stacked pull request names what it
waits on, next to the branch it targets: `by alice · feature-a (PR #12) ←
feature-b`.

**Approve** posts an approving review through the API and then removes the card.
**Dismiss** only removes the card: nothing is sent to GitHub, and the pull
request keeps whatever review state it already had. Both leave the list shorter,
which is the point.

The list of pull requests survives a reload. **Clear** empties it.

## Slack (optional)

smashtb can listen to Slack over Socket Mode. The `slack` badge in the top
right says whether it is connected. Once it is, **Configure Slack** next to it
opens a two-step page:

1. **Channels**: every channel you are a member of, with a box to tick for
   each one to watch.
2. **Pattern**: a JavaScript regular expression. A new message in a watched
   channel is picked up when it matches. To help get it right, the page runs it
   on the last 16 messages of each watched channel, and on the replies in the
   latest 8 threads among them, and highlights what matches.

Both are saved in the browser as you change them, so a restart keeps them, and
opening the page again shows what is in force.

When a new message matches, from anyone but you:

- **A browser notification** shows the channel, the author and the start of
  the message. Clicking it opens the message in Slack. Press **Enable
  notifications** next to the badge once to allow them. A new notification
  from a channel replaces the previous one from that channel rather than
  piling up.
- **An entry in History**, marked with the Slack icon, keeps the message, so
  nothing is lost if the notification goes unseen. Clicking it loads the pull
  requests the message links to, or opens it in Slack if it links to none.

Only the tab holding the Slack connection notifies, so keep one smashtb tab
open; it can be in the background.

It needs a Slack app of your own. At https://api.slack.com/apps choose
**Create New App**, then **From a manifest**, and paste:

```yaml
display_information:
  name: smashtb
  description: Notifies me of Slack messages matching a pattern
oauth_config:
  scopes:
    user:
      - channels:read
      - channels:history
      - groups:read
      - groups:history
      - users:read
settings:
  event_subscriptions:
    user_events:
      - message.channels
      - message.groups
  socket_mode_enabled: true
  org_deploy_enabled: false
  token_rotation_enabled: false
```

Then:

1. **OAuth & Permissions**, **Install to Workspace**: copy the *User OAuth
   Token* (`xoxp-...`) into `SLACK_APP_OAUTH_TOKEN`.
2. **Basic Information**, **App-Level Tokens**, **Generate Token and Scopes**,
   with the `connections:write` scope: copy the token (`xapp-...`) into
   `SLACK_APP_APP_LEVEL_TOKEN`.

Neither token reaches the page. slack.com does not let a browser send it an
`Authorization` header, so the server started by `run.sh` makes the Slack Web
API calls itself, for a short list of read-only methods, and only answers
requests carrying the app's own header from its own origin. The page then opens
the Socket Mode WebSocket directly.

Slack hands each event to just one of an app's connections, so only one tab
connects at a time. The others say *Slack is connected in another tab*, and
one of them takes over when that tab closes. **Use Slack here**, next to the
badge, moves the connection to the current tab straight away.

## Notes and limits

- Diffs come from the pull request files endpoint, which sends a patch per
  file. Binary files and diffs GitHub considers too large arrive without a
  patch; the app says so instead of showing an empty diff.
- At most 1000 files are fetched per pull request, ten pages of a hundred.
  Beyond that the card is marked *file list truncated*.
- The token is held in the page and in local storage, and is sent only to
  `api.github.com`. Anyone who can open your browser profile can read it, so
  prefer a short-lived fine-grained token.
