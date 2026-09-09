# smashtb

A single-page app for working through a batch of GitHub pull requests. Paste
the links, read the diffs, approve or dismiss each one.

Written in OCaml 5.5.1 and compiled to JavaScript with js_of_ocaml, using
[brr](https://erratique.ch/software/brr) for the DOM and fetch bindings.

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

**Approve** posts an approving review through the API and then removes the card.
**Dismiss** only removes the card: nothing is sent to GitHub, and the pull
request keeps whatever review state it already had. Both leave the list shorter,
which is the point.

The list of pull requests survives a reload. **Clear** empties it.

## Notes and limits

- Diffs come from the pull request files endpoint, which sends a patch per
  file. Binary files and diffs GitHub considers too large arrive without a
  patch; the app says so instead of showing an empty diff.
- At most 1000 files are fetched per pull request, ten pages of a hundred.
  Beyond that the card is marked *file list truncated*.
- The token is held in the page and in local storage, and is sent only to
  `api.github.com`. Anyone who can open your browser profile can read it, so
  prefer a short-lived fine-grained token.
