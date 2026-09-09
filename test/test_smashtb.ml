(* Unit tests for the parts that do not need a browser: pull request URL
   recognition and unified-diff parsing.  Run with `dune test`. *)

open Smashtb

let failures = ref 0

let check name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL %s\n" name
  end

let show = function
  | None -> "none"
  | Some r -> Pr_url.to_string r

let url name input expected =
  let got = show (Pr_url.parse input) in
  if got <> expected then begin
    incr failures;
    Printf.printf "FAIL %s: %S -> %s, expected %s\n" name input got expected
  end

let () =
  url "repo url with an anchor is not a pr" "https://github.com/smelc/smelc#1" "none";
  url "pull url" "https://github.com/smelc/smelc/pull/42" "smelc/smelc#42";
  url "files tab" "https://github.com/smelc/smelc/pull/42/files" "smelc/smelc#42";
  url "comment anchor" "https://github.com/o/r/pull/7#issuecomment-12345" "o/r#7";
  url "query string" "https://github.com/o/r/pull/7?w=1" "o/r#7";
  url "no scheme" "github.com/o/r/pull/7" "o/r#7";
  url "api url" "https://api.github.com/repos/o/r/pulls/7" "o/r#7";
  url "short form" "o/r#7" "o/r#7";
  url "surrounding space" "   o/r#7\t" "o/r#7";
  url "trailing slash" "https://github.com/o/r/pull/7/" "o/r#7";
  url "dotted repo" "https://github.com/o/r.js/pull/7" "o/r.js#7";
  url "issue, not a pr" "https://github.com/o/r/issues/7" "none";
  url "repo only" "https://github.com/o/r" "none";
  url "not a number" "https://github.com/o/r/pull/abc" "none";
  url "zero" "o/r#0" "none";
  url "junk" "hello" "none";
  url "empty" "" "none";

  (* Extraction from free-form text: everything that is not a pull request
     reference is ignored, and each one is kept once, in order of appearance. *)
  let extracted text = List.map Pr_url.to_string (Pr_url.extract text) in
  let same name text expected =
    let got = extracted text in
    if got <> expected then begin
      incr failures;
      Printf.printf "FAIL %s: %S -> [%s], expected [%s]\n" name text (String.concat "; " got)
        (String.concat "; " expected)
    end
  in
  same "one per line" "o/r#1\nhttps://github.com/o/r/pull/2\no/r#3" [ "o/r#1"; "o/r#2"; "o/r#3" ];
  same "duplicates collapse"
    "https://github.com/o/r/pull/2 and again https://github.com/o/r/pull/2" [ "o/r#2" ];
  same "same pr in two spellings" "o/r#2 https://github.com/o/r/pull/2" [ "o/r#2" ];
  same "prose is ignored" "nothing to see here" [];
  same "trailing full stop"
    "please review https://github.com/o/r/pull/9." [ "o/r#9" ];
  same "wrapped in a sentence"
    "Can someone look at https://github.com/o/r/pull/9, it blocks the release?" [ "o/r#9" ];
  same "slack angle brackets" "<https://github.com/o/r/pull/9>" [ "o/r#9" ];
  same "slack link with a label" "<https://github.com/o/r/pull/9|PR 9 here>" [ "o/r#9" ];
  same "markdown link" "[the fix](https://github.com/o/r/pull/9)" [ "o/r#9" ];
  same "parenthesised" "(https://github.com/o/r/pull/9)" [ "o/r#9" ];
  same "backticks" "`https://github.com/o/r/pull/9`" [ "o/r#9" ];
  same "files tab and anchor keep working"
    "https://github.com/o/r/pull/9/files#diff-abc123" [ "o/r#9" ];
  same "issues are not pull requests" "https://github.com/o/r/issues/9" [];
  same "other links are ignored"
    "see https://example.com/a/b/pull/1x and https://github.com/o/r" [];

  (* The shape this is really for: a pasted Slack message. *)
  let slack =
    "Alice  10:32\n\
     hey team, can I get eyes on <https://github.com/acme/widgets/pull/12|widgets#12>?\n\
     it depends on acme/core#88, and the deploy notes are at\n\
     https://wiki.example.com/deploys (nothing to review there)\n\
     Bob  10:35\n\
     looking. also https://github.com/acme/widgets/pull/12/files is easier to read"
  in
  same "slack message" slack [ "acme/widgets#12"; "acme/core#88" ];

  (* A two-hunk patch: line numbers must restart at each hunk header, additions
     must only advance the new-side counter, deletions only the old side. *)
  let patch =
    String.concat "\n"
      [
        "@@ -1,3 +1,4 @@";
        " let a = 1";
        "-let b = 2";
        "+let b = 3";
        "+let c = 4";
        " let d = 5";
        "@@ -20,2 +21,1 @@ let f x =";
        "-  gone";
        " kept";
        "\\ No newline at end of file";
      ]
  in
  let lines = Diff.parse patch in
  let kinds = List.map (fun (l : Diff.line) -> l.kind) lines in
  check "line count" (List.length lines = 10);
  check "kinds"
    (kinds
    = Diff.[ Hunk; Context; Removed; Added; Added; Context; Hunk; Removed; Context; Meta ]);
  let numbers =
    List.map (fun (l : Diff.line) -> (l.old_no, l.new_no)) lines
  in
  check "numbering"
    (numbers
    = [
        (None, None);
        (Some 1, Some 1);
        (Some 2, None);
        (None, Some 2);
        (None, Some 3);
        (Some 3, Some 4);
        (None, None);
        (Some 20, None);
        (Some 21, Some 21);
        (None, None);
      ]);
  let texts = List.map (fun (l : Diff.line) -> l.text) lines in
  check "markers stripped from content"
    (List.nth texts 2 = "let b = 2" && List.nth texts 3 = "let b = 3");
  check "hunk header kept verbatim" (List.nth texts 0 = "@@ -1,3 +1,4 @@");

  (* Hunks may omit the count when it is 1. *)
  let short = Diff.parse "@@ -5 +6 @@\n+x" in
  check "short hunk header"
    (match short with
    | [ _; { Diff.new_no = Some 6; _ } ] -> true
    | _ -> false);

  check "empty patch" (Diff.parse "" = []);

  if !failures = 0 then print_endline "all tests passed"
  else begin
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1
  end
