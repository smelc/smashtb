(* smashtb: paste a list of GitHub pull request links, read the diffs, approve
   or dismiss.  Approving posts a review through the API; dismissing only drops
   the pull request from this page. *)

open Brr
module Diff = Smashtb.Diff
module Pr_order = Smashtb.Pr_order
open Ui

let no_token_msg =
  "No GITHUB_TOKEN. Start the app with ./run.sh so a token is passed in."

module Urls = Map.Make (String)

let history_key = "smashtb.history"

(* How many entries the history keeps, most recent first. *)
let history_limit = 8
let keep_recent entries = List.filteri (fun i _ -> i < history_limit) entries

(* {1 History}

   The pull request list itself is deliberately not restored: a new session
   starts empty. What is kept is the last few pastes that yielded something,
   and the Slack messages that matched the pattern, so a batch can be put back
   with one click. *)

(* Where a Slack message came from, for an entry made from one. *)
type slack_origin = {
  channel : string;  (** the channel's name *)
  author : string;
  link : string;  (** the message's address in Slack *)
}

type entry = {
  source : string;
      (** the text that was pasted, or the Slack message, kept for recognising
          the entry *)
  refs : Gh.pr_ref list;  (** what was extracted from it *)
  slack : slack_origin option;
}

let entry_to_jv e =
  Jv.obj
    [|
      ("source", Jv.of_string e.source);
      ("refs", Jv.of_list Jv.of_string (List.map Gh.ref_url e.refs));
      ( "slack",
        match e.slack with
        | None -> Jv.null
        | Some o ->
            Jv.obj
              [|
                ("channel", Jv.of_string o.channel);
                ("author", Jv.of_string o.author);
                ("link", Jv.of_string o.link);
              |] );
    |]

let entry_of_jv j =
  let str v = if Jv.is_none v then "" else Jstr.to_string (Jv.to_jstr v) in
  let refs = Jv.get j "refs" in
  let slack = Jv.get j "slack" in
  {
    source = str (Jv.get j "source");
    refs =
      (if Jv.is_none refs then []
       else
         List.filter_map
           (fun v -> Gh.parse_ref (str v))
           (Jv.to_list Fun.id refs));
    slack =
      (if Jv.is_none slack then None
       else
         Some
           {
             channel = str (Jv.get slack "channel");
             author = str (Jv.get slack "author");
             link = str (Jv.get slack "link");
           });
  }

let load_history () =
  match
    Option.map (fun s -> Json.decode (Jstr.v s)) (Store.get history_key)
  with
  | None | Some (Error _) -> []
  | Some (Ok j) ->
      if Jv.is_none j then []
      else
        (* Trimmed too, for a history saved when the limit was higher. *)
        keep_recent
          (List.filter
             (fun e -> e.refs <> [] || e.slack <> None)
             (Jv.to_list entry_of_jv j))

let save_history entries =
  Store.set history_key
    (Jstr.to_string (Json.encode (Jv.of_list entry_to_jv entries)))

(* How many references a hover lists before it stops. *)
let hover_limit = 16

let hover_text refs =
  let shown = List.filteri (fun i _ -> i < hover_limit) refs in
  let hidden = List.length refs - List.length shown in
  let lines = List.map Gh.ref_to_string shown in
  let lines =
    if hidden = 0 then lines
    else lines @ [ Printf.sprintf "and %d more" hidden ]
  in
  String.concat "\n" lines

(* A one-line gist of the pasted text, for telling entries apart. *)
let excerpt text =
  let flat =
    String.map
      (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c)
      text
  in
  let rec squeeze acc prev_space i =
    if i >= String.length flat then acc
    else
      let c = flat.[i] in
      if c = ' ' then
        squeeze (if prev_space then acc else acc ^ " ") true (i + 1)
      else squeeze (acc ^ String.make 1 c) false (i + 1)
  in
  let one_line = String.trim (squeeze "" true 0) in
  if String.length one_line <= 90 then one_line
  else String.sub one_line 0 89 ^ "\xe2\x80\xa6"

(* {1 Token} *)

(* The token arrives one way only: ./run.sh writes the value of $GITHUB_TOKEN
   into config.js as window.SMASHTB_GITHUB_TOKEN.  There is nowhere to type one
   in, so a page served without that file can do nothing until it is restarted
   with a token. *)
let token_from_page () =
  let v = Jv.get Jv.global "SMASHTB_GITHUB_TOKEN" in
  if Jv.is_none v then "" else String.trim (Jstr.to_string (Jv.to_jstr v))

(* {1 Token badge}

   The badge in the top right says whether the token has been shown to work.
   Nothing here trusts the token because it looks well formed: it says [Works]
   only once GitHub has answered a real request, and flips to [Broken] as soon
   as GitHub turns one down for authentication. *)

type token_state =
  | Untested of string  (** why we cannot say yet *)
  | Works of string option
      (** the login GitHub reported, when we asked for it *)
  | Broken of string  (** what went wrong *)

(* [works_tip who] is the tooltip once the token is known to work, [who]
   being the account it belongs to when we asked. *)
let token_badge ~label ~works_tip () =
  let label = El.span ~at:[ cls "tok-label" ] [ txt label ] in
  let glyph = El.span ~at:[ cls "tok-glyph" ] [] in
  let el = El.div ~at:[ cls "token-badge" ] [ label; glyph ] in
  let state = ref (Untested "") in
  let set s =
    state := s;
    let mark, style, tip =
      match s with
      | Untested why -> ("\xe2\x9d\x93", "tok-untested", why)
      | Works who -> ("\xe2\x9c\x85", "tok-works", works_tip who)
      | Broken why -> ("\xe2\x9d\x8c", "tok-broken", why)
    in
    El.set_children glyph [ txt mark ];
    El.set_at (Jstr.v "class") (Some (Jstr.v ("token-badge " ^ style))) el;
    El.set_at (Jstr.v "title") (Some (Jstr.v tip)) el
  in
  (el, set, state)

(* {1 Status icons}

   Brr creates elements in the HTML namespace, and an <svg> only renders in the
   SVG one, so the markup is set as innerHTML. Both strings are constants
   written here, never anything that came back from GitHub. *)

let merged_svg =
  "<svg viewBox=\"0 0 16 16\" width=\"15\" height=\"15\" \
   aria-hidden=\"true\"><path d=\"M4.5 5.5 V10.5 M4.5 6.4 C4.5 8.4 6.7 8 9.6 \
   8\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.7\" \
   stroke-linecap=\"round\"/><circle cx=\"4.5\" cy=\"3.5\" r=\"1.9\" \
   fill=\"currentColor\"/><circle cx=\"4.5\" cy=\"12.5\" r=\"1.9\" \
   fill=\"currentColor\"/><circle cx=\"11.5\" cy=\"8\" r=\"1.9\" \
   fill=\"currentColor\"/></svg>"

let approved_svg =
  "<svg viewBox=\"0 0 16 16\" width=\"15\" height=\"15\" \
   aria-hidden=\"true\"><circle cx=\"8\" cy=\"8\" r=\"6.6\" fill=\"none\" \
   stroke=\"currentColor\" stroke-width=\"1.6\"/><path d=\"M4.9 8.2 6.9 10.3 \
   11.1 5.9\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" \
   stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>"

let set_svg el markup = Jv.set (El.to_jv el) "innerHTML" (Jv.of_string markup)

let icon ~kind ~tip markup =
  let el =
    El.span ~at:[ cls ("pr-status " ^ kind); At.title (Jstr.v tip) ] []
  in
  set_svg el markup;
  el

(* {1 Copying a branch name}

   The two sheets of paper everything uses for "copy": this is GitHub's own
   copy icon, and the tick that replaces it for a moment afterwards. *)

let copy_svg =
  "<svg viewBox=\"0 0 16 16\" width=\"13\" height=\"13\" \
   aria-hidden=\"true\"><path fill=\"currentColor\" d=\"M0 6.75C0 5.784.784 5 \
   1.75 5h1.5a.75.75 0 0 1 0 1.5h-1.5a.25.25 0 0 0-.25.25v7.5c0 \
   .138.112.25.25.25h7.5a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 \
   1.75 0 0 1 9.25 16h-7.5A1.75 1.75 0 0 1 0 14.25Z\"/><path \
   fill=\"currentColor\" d=\"M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 \
   .784 16 1.75v7.5A1.75 1.75 0 0 1 14.25 11h-7.5A1.75 1.75 0 0 1 5 \
   9.25Zm1.75-.25a.25.25 0 0 0-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 0 0 \
   .25-.25v-7.5a.25.25 0 0 0-.25-.25Z\"/></svg>"

let copied_svg =
  "<svg viewBox=\"0 0 16 16\" width=\"13\" height=\"13\" \
   aria-hidden=\"true\"><path d=\"M3.4 8.6 6.4 11.6 12.6 4.7\" fill=\"none\" \
   stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" \
   stroke-linejoin=\"round\"/></svg>"

(* navigator.clipboard only exists in a secure context, which http://localhost
   is; a page served any other way over http has none, and the button says so
   rather than failing silently. *)
let clipboard () =
  let c = Jv.get (Navigator.to_jv G.navigator) "clipboard" in
  if Jv.is_none c then None
  else Some (Brr_io.Clipboard.of_navigator G.navigator)

(* Publish [el]'s height as --top-h on the root element, and keep it current
   as it changes (the input can be resized, the status line can wrap). The pull
   request headers stick right below it. *)
let track_height el =
  let set () =
    El.set_inline_style (Jstr.v "--top-h")
      (Jstr.v (Printf.sprintf "%gpx" (El.bound_h el)))
      (Document.root G.document)
  in
  set ();
  let ro = Jv.get Jv.global "ResizeObserver" in
  if not (Jv.is_none ro) then
    let obs = Jv.new' ro [| Jv.callback ~arity:1 (fun _ -> set ()) |] in
    ignore (Jv.call obs "observe" [| El.to_jv el |])

let copy_button text =
  let tip = Printf.sprintf "Copy %s to the clipboard" text in
  let b =
    El.button
      ~at:[ cls "copy-btn"; attr "type" "button"; At.title (Jstr.v tip) ]
      []
  in
  set_svg b copy_svg;
  let timer = ref None in
  let reset () =
    set_svg b copy_svg;
    El.set_class (Jstr.v "copied") false b;
    El.set_class (Jstr.v "failed") false b;
    El.set_at (Jstr.v "title") (Some (Jstr.v tip)) b
  in
  (* The button says what happened for a moment, then goes back to offering
     the copy again. *)
  let flash outcome said =
    Option.iter G.stop_timer !timer;
    El.set_class (Jstr.v outcome) true b;
    El.set_at (Jstr.v "title") (Some (Jstr.v said)) b;
    if outcome = "copied" then set_svg b copied_svg;
    timer := Some (G.set_timeout ~ms:1400 reset)
  in
  on_click b (fun ev ->
      Ev.stop_propagation ev;
      Ev.prevent_default ev;
      match clipboard () with
      | None ->
          flash "failed" "This browser will not give the page a clipboard."
      | Some clip ->
          Fut.await
            (Brr_io.Clipboard.write_text clip (Jstr.v text))
            (function
              | Ok () -> flash "copied" (Printf.sprintf "Copied %s" text)
              | Error e ->
                  flash "failed"
                    ("Could not copy: " ^ Jstr.to_string (Jv.Error.message e))));
  b

(* A branch name as it appears in a card's second line. The default branch is
   the one nobody needs to copy: it is where everything lands anyway. *)
let branch_els ~default_branch name =
  txt name
  :: (if name = "" || name = default_branch then [] else [ copy_button name ])

(* {1 Diff view} *)

let render_diff (f : Gh.file) =
  match f.patch with
  | None ->
      let why =
        match f.status with
        | "removed" ->
            "File deleted. GitHub does not send a patch for deletions of large \
             files."
        | "renamed" -> "Renamed with no content change."
        | _ ->
            "No diff available: the file is binary, or its diff is too large \
             for the API."
      in
      El.div ~at:[ cls "diff-empty" ] [ txt why ]
  | Some patch ->
      let num = function None -> "" | Some n -> string_of_int n in
      let row (l : Diff.line) =
        let kind_cls, marker =
          match l.kind with
          | Diff.Added -> ("l-add", "+")
          | Diff.Removed -> ("l-del", "-")
          | Diff.Context -> ("l-ctx", " ")
          | Diff.Hunk -> ("l-hunk", "")
          | Diff.Meta -> ("l-meta", "")
        in
        El.div
          ~at:[ cls ("dl " ^ kind_cls) ]
          [
            El.span ~at:[ cls "ln" ] [ txt (num l.old_no) ];
            El.span ~at:[ cls "ln" ] [ txt (num l.new_no) ];
            El.span ~at:[ cls "mk" ] [ txt marker ];
            El.span ~at:[ cls "lc" ] [ txt l.text ];
          ]
      in
      El.div ~at:[ cls "diff" ] (List.map row (Diff.parse patch))

(* {1 One file, foldable} *)

let render_file (f : Gh.file) =
  let body = El.div ~at:[ cls "file-body" ] [] in
  (* Diffs are only built when the file is first expanded: a pull request can
     carry hundreds of files and rendering them all up front is slow. *)
  let built = ref false in
  let build () =
    if not !built then begin
      built := true;
      El.set_children body [ render_diff f ]
    end
  in
  let name =
    match f.previous_filename with
    | Some old when old <> f.filename -> old ^ " \xe2\x86\x92 " ^ f.filename
    | _ -> f.filename
  in
  let head =
    summary
      ~at:[ cls "file-head" ]
      [
        El.span ~at:[ cls "chev" ] [ txt "\xe2\x96\xb8" ];
        El.span ~at:[ cls ("fstatus s-" ^ f.status) ] [ txt f.status ];
        El.span ~at:[ cls "fname" ] [ txt name ];
        El.span
          ~at:[ cls "stat add" ]
          [ txt (Printf.sprintf "+%d" f.additions) ];
        El.span
          ~at:[ cls "stat del" ]
          [ txt (Printf.sprintf "-%d" f.deletions) ];
        El.a
          ~at:
            [
              cls "blob";
              At.href (Jstr.v f.blob_url);
              attr "target" "_blank";
              attr "rel" "noreferrer";
              At.title (Jstr.v "Open this file on GitHub");
            ]
          [ txt "view" ];
      ]
  in
  on_click head (fun _ -> build ());
  let el = details ~at:[ cls "file" ] [ head; body ] in
  (el, build)

(* {1 One pull request, foldable} *)

(* The branch a pull request lands on. When that branch is carried by another
   pull request on the page, its number is shown next to it, so that a stack
   can be read off the line: [set_parent] is called again every time the page
   learns of another pull request. *)
let render_base (pr : Gh.pr) =
  let name () = branch_els ~default_branch:pr.default_branch pr.base in
  let el = El.span ~at:[ cls "pr-base" ] (name ()) in
  let set_parent (p : Pr_order.t option) =
    El.set_children el
      (name ()
      @
      match p with
      | None -> []
      | Some p ->
          [
            El.span
              ~at:
                [
                  cls "pr-parent";
                  At.title
                    (Jstr.v
                       (Printf.sprintf
                          "%s is the branch of %s/%s#%d, which is on this page \
                           too: this pull request only lands once that one \
                           does."
                          pr.base p.owner p.repo p.number));
                ]
              [ txt (Printf.sprintf " (PR #%d)" p.number) ];
          ])
  in
  (el, set_parent)

let render_pr ~token ~(on_api : (unit, string) result -> unit)
    ~(on_gone : Gh.pr_ref -> unit) (pr : Gh.pr) =
  let files = List.map render_file pr.files in
  let file_els = List.map fst files in
  let n = List.length pr.files in
  let error_box = El.div ~at:[ cls "error hidden" ] [] in
  let card = details ~at:[ cls "pr"; attr "open" "" ] [] in

  let set_all_files b =
    List.iter
      (fun (el, build) ->
        if b then build ();
        set_open el b)
      files
  in
  let files_block =
    details
      ~at:[ cls "files"; attr "open" "" ]
      [
        summary
          ~at:[ cls "files-head" ]
          [
            El.span ~at:[ cls "chev" ] [ txt "\xe2\x96\xb8" ];
            El.span
              ~at:[ cls "files-title" ]
              [
                txt (Printf.sprintf "%d %s changed" n (plural n "file" "files"));
              ];
            button ~classes:"btn tiny" "expand diffs" (fun () ->
                set_all_files true);
            button ~classes:"btn tiny" "collapse diffs" (fun () ->
                set_all_files false);
          ];
        El.div ~at:[ cls "file-list" ] file_els;
      ]
  in

  let show_error msg =
    El.set_children error_box [ txt msg ];
    El.set_class (Jstr.v "hidden") false error_box
  in
  let clear_error () = El.set_class (Jstr.v "hidden") true error_box in
  let gone () =
    El.remove card;
    on_gone pr.pr_ref
  in

  (* Nothing left to approve: either it is already in, or someone's approval
     already stands. *)
  let settled =
    if pr.merged then Some ("merged", "This pull request is already merged.")
    else
      match pr.approved_by with
      | [] -> None
      | who ->
          Some
            ( "approved",
              Printf.sprintf "Already approved by %s." (String.concat ", " who)
            )
  in
  let approve_btn = ref (El.div []) in
  let dismiss_btn = ref (El.div []) in
  let busy b =
    set_disabled !approve_btn b;
    set_disabled !dismiss_btn b
  in
  approve_btn :=
    button ~classes:"btn primary"
      ~title:
        "Post an approving review on GitHub, then remove this PR from the list"
      "Approve" (fun () ->
        clear_error ();
        busy true;
        El.set_children !approve_btn [ txt "Approving\xe2\x80\xa6" ];
        Fut.await (Gh.approve ~token pr.pr_ref) (function
          | Ok () ->
              on_api (Ok ());
              gone ()
          | Error e ->
              on_api (Error e);
              busy false;
              El.set_children !approve_btn [ txt "Approve" ];
              show_error ("Could not approve: " ^ e)));
  (match settled with
  | None -> ()
  | Some (_, tip) ->
      set_disabled !approve_btn true;
      El.set_at (Jstr.v "title") (Some (Jstr.v tip)) !approve_btn);
  dismiss_btn :=
    button ~classes:"btn"
      ~title:
        "Remove this PR from the list without approving it. Nothing is sent to \
         GitHub." "Dismiss" (fun () -> gone ());

  let base_el, set_parent = render_base pr in
  let badges =
    List.filter_map
      (fun (b, label, c) ->
        if b then Some (El.span ~at:[ cls ("badge " ^ c) ] [ txt label ])
        else None)
      [
        (pr.draft, "draft", "b-draft");
        (pr.state = "closed" && not pr.merged, "closed", "b-closed");
        (pr.truncated, "file list truncated", "b-warn");
      ]
  in
  let head =
    summary
      ~at:[ cls "pr-head" ]
      [
        El.span ~at:[ cls "chev" ] [ txt "\xe2\x96\xb8" ];
        El.div
          ~at:[ cls "pr-ident" ]
          [
            El.div
              ~at:[ cls "pr-line1" ]
              (El.a
                 ~at:
                   [
                     cls "pr-num";
                     At.href (Jstr.v pr.html_url);
                     attr "target" "_blank";
                     attr "rel" "noreferrer";
                   ]
                 [ txt (Gh.ref_to_string pr.pr_ref) ]
              :: El.span ~at:[ cls "pr-title" ] [ txt pr.title ]
              :: badges);
            El.div
              ~at:[ cls "pr-line2" ]
              [
                txt (Printf.sprintf "by %s \xc2\xb7 " pr.author);
                base_el;
                txt " \xe2\x86\x90 ";
                El.span
                  ~at:[ cls "pr-branch" ]
                  (branch_els ~default_branch:pr.default_branch pr.head);
                txt " \xc2\xb7 ";
                El.span
                  ~at:[ cls "stat add" ]
                  [ txt (Printf.sprintf "+%d" pr.additions) ];
                El.span
                  ~at:[ cls "stat del" ]
                  [ txt (Printf.sprintf "-%d" pr.deletions) ];
              ];
          ];
        (match settled with
        | None -> El.span ~at:[ cls "pr-status empty" ] []
        | Some ("merged", tip) -> icon ~kind:"merged" ~tip merged_svg
        | Some (_, tip) -> icon ~kind:"approved" ~tip approved_svg);
        El.div ~at:[ cls "actions" ] [ !dismiss_btn; !approve_btn ];
      ]
  in
  El.set_children card
    [ head; El.div ~at:[ cls "pr-body" ] [ error_box; files_block ] ];
  (card, set_parent)

(* {1 Shell} *)

let loading_card (r : Gh.pr_ref) =
  El.div
    ~at:[ cls "pr placeholder" ]
    [ txt (Printf.sprintf "Loading %s\xe2\x80\xa6" (Gh.ref_to_string r)) ]

(* One pull request's place in the list: the element it lives in, where it
   belongs in the merge order, and how to tell it which pull request carries
   the branch it targets. Everything but the element is provisional until
   GitHub has answered. *)
type slot = {
  el : El.t;
  order : Pr_order.t;
  set_parent : Pr_order.t option -> unit;
}

let order_of_pr (pr : Gh.pr) =
  {
    Pr_order.owner = pr.pr_ref.owner;
    repo = pr.pr_ref.repo;
    number = pr.pr_ref.number;
    base = pr.base;
    head = pr.head;
    base_is_default = String.equal pr.base pr.default_branch;
  }

(* {1 Slack}

   Connects, and counts the messages from watched channels that match the
   pattern, logging each to the console. Both are set on the Slack page. *)

(* Slack's mark, roughly: Brr makes HTML elements, and an <svg> only renders in
   the SVG namespace, hence the markup string. A constant, never anything that
   came from Slack. *)
let slack_svg =
  "<svg viewBox=\"0 0 16 16\" width=\"13\" height=\"13\" \
   aria-hidden=\"true\"><rect x=\"4.3\" y=\"1\" width=\"2.6\" height=\"14\" \
   rx=\"1.3\" fill=\"#36C5F0\"/><rect x=\"9.1\" y=\"1\" width=\"2.6\" \
   height=\"14\" rx=\"1.3\" fill=\"#2EB67D\"/><rect x=\"1\" y=\"4.3\" \
   width=\"14\" height=\"2.6\" rx=\"1.3\" fill=\"#E01E5A\" \
   fill-opacity=\".9\"/><rect x=\"1\" y=\"9.1\" width=\"14\" height=\"2.6\" \
   rx=\"1.3\" fill=\"#ECB22E\" fill-opacity=\".9\"/></svg>"

(* [on_match me m ~channel] is told about every message, not written by the
   user, from a watched channel that matches the pattern, [channel] being its
   name. The result redraws the badge, whose text says what [config] watches:
   call it after changing [config]. *)
let start_slack ~me ~config ~on_match set_state badge =
  if not (Slack.configured ()) then begin
    El.set_class (Jstr.v "hidden") true badge;
    ignore
  end
  else begin
    (* What auth.test said about the user token: unknown until it answers.
       A refusal outranks a working socket, which only proves the app-level
       token. *)
    let who = ref None in
    let matched = ref 0 in
    let connected () =
      match !who with
      | Some (Error e) -> Broken ("Slack turned the user token down: " ^ e)
      | _ ->
          let as_ =
            match !who with
            | Some (Ok (me : Slack.me)) when me.user <> "" -> " as " ^ me.user
            | _ -> ""
          in
          let c = !config in
          let n = List.length (Slack_config.watched_anywhere c) in
          let watching =
            if n = 0 then "No channel is watched yet."
            else if String.trim (Slack_config.pattern c) = "" then
              "No pattern is set yet."
            else
              Printf.sprintf "%d %s matched so far in %d watched %s." !matched
                (plural !matched "message" "messages")
                n
                (plural n "channel" "channels")
          in
          let notifications =
            match Notify.permission () with
            | Granted -> ""
            | Ask -> " Notifications are off: press Enable notifications."
            | Denied ->
                " Notifications are blocked for this page: allow them in the \
                 browser's settings for this site."
            | Unsupported -> " This browser cannot show notifications."
          in
          Works
            (Some
               (Printf.sprintf "Connected to Slack%s. %s%s" as_ watching
                  notifications))
    in
    let take_over = ref ignore in
    let here =
      button ~classes:"btn tiny"
        ~title:
          "Move the Slack connection to this tab. The tab that has it now \
           waits instead." "Use Slack here" (fun () -> !take_over ())
    in
    let configure =
      button ~classes:"btn tiny"
        ~title:"Choose the channels to watch and the pattern to look for"
        "Configure Slack \xe2\x9a\x99\xef\xb8\x8f" (fun () ->
          Slack_page.go_to Channels)
    in
    let state = ref Slack.Connecting in
    let show_ref = ref (fun _ -> ()) in
    let enable =
      button ~classes:"btn tiny"
        ~title:"Let this page notify you of the Slack messages that match"
        "Enable notifications \xf0\x9f\x94\x94" (fun () ->
          Notify.request (fun _ -> !show_ref !state))
    in
    El.set_class (Jstr.v "hidden") true here;
    El.set_class (Jstr.v "hidden") true configure;
    El.set_class (Jstr.v "hidden") true enable;
    El.insert_siblings `After badge [ here; enable; configure ];
    let show s =
      state := s;
      El.set_class (Jstr.v "hidden")
        (not (s = Slack.Connected && Notify.permission () = Ask))
        enable;
      El.set_class (Jstr.v "hidden") (s <> Slack.Other_tab) here;
      El.set_class (Jstr.v "hidden")
        (match (s, !who) with
        | Slack.Connected, Some (Ok _) -> false
        | _ -> true)
        configure;
      set_state
        (match s with
        | Slack.Other_tab ->
            Untested
              "Slack is connected in another tab. This one takes over when \
               that tab closes."
        | Connecting -> Untested "Connecting to Slack\xe2\x80\xa6"
        | Connected -> connected ()
        | Retrying { why; seconds } ->
            Broken
              (Printf.sprintf "Slack: %s. Trying again in %d s." why seconds))
    in
    show_ref := show;
    Fut.await me (fun r ->
        who := Some r;
        show !state);
    let slack =
      Slack.start ~on_state:show ~on_message:(fun (m : Slack.message) ->
          let c = !config in
          let me = match !who with Some (Ok me) -> Some me | _ -> None in
          let mine =
            match me with Some me -> me.user_id = m.user | None -> false
          in
          if
            (not mine)
            && Slack_config.is_watched c ~team:m.team m.channel
            && Slack_config.matches c m.text
          then begin
            incr matched;
            show !state;
            let channel =
              match
                List.find_opt
                  (fun (ch : Slack_config.channel) -> ch.id = m.channel)
                  (Slack_config.watched c ~team:m.team)
              with
              | Some ch -> ch.name
              | None -> m.channel
            in
            on_match me m ~channel
          end)
    in
    take_over := slack.take_over;
    fun () -> show !state
  end

let () =
  let doc_body = Document.body G.document in

  (* --- state --- *)
  let token = token_from_page () in
  let badge, set_token_state, token_status =
    token_badge ~label:"github"
      ~works_tip:(function
        | None -> "GitHub accepted this token."
        | Some login ->
            Printf.sprintf
              "GitHub accepted this token. Approvals are posted as %s." login)
      ()
  in
  let slack_badge, set_slack_state, _ =
    token_badge ~label:"slack"
      ~works_tip:(function None -> "Connected to Slack." | Some who -> who)
      ()
  in
  (* url -> the slot showing that pull request, so a card can be found again to
     replace or remove it. *)
  let loaded : slot Urls.t ref = ref Urls.empty in
  (* Fetches in flight, so the "Loading..." line can clear once they settle. *)
  let pending = ref 0 in
  let list_el = El.div ~at:[ cls "list" ] [] in
  let history_el = El.div ~at:[ cls "history" ] [] in
  let history = ref (load_history ()) in
  let status = El.div ~at:[ cls "status" ] [] in
  let empty_hint =
    El.div
      ~at:[ cls "hint" ]
      [
        txt "Paste anything holding pull request links above, then press Load.";
      ]
  in

  let say ?(bad = false) msg =
    El.set_children status [ txt msg ];
    El.set_class (Jstr.v "bad") bad status
  in

  let hist_head = El.div ~at:[ cls "hist-head" ] [] in
  let refresh_list () =
    El.set_class (Jstr.v "hidden") (not (Urls.is_empty !loaded)) empty_hint
  in

  (* Lay the list out in merge order, and tell each pull request which loaded
     one, if any, carries the branch it targets. Both answers depend on the
     whole list, so this runs again every time a pull request arrives. *)
  let reorder () =
    let slots = List.map snd (Urls.bindings !loaded) in
    let orders = List.map (fun s -> s.order) slots in
    List.iter (fun s -> s.set_parent (Pr_order.parent orders s.order)) slots;
    El.set_children list_el
      (List.map (fun s -> s.el) (Pr_order.sort (fun s -> s.order) slots))
  in

  (* Dropping a pull request leaves the others where they are: re-deriving the
     order here would shuffle anything stacked on the one that just went. *)
  let forget r =
    let key = Gh.ref_url r in
    (match Urls.find_opt key !loaded with
    | Some slot -> El.remove slot.el
    | None -> ());
    loaded := Urls.remove key !loaded;
    refresh_list ();
    say (Printf.sprintf "%s removed from the list." (Gh.ref_to_string r))
  in

  (* Every answer from GitHub is evidence about the token: any success proves
     it, any authentication failure disproves it.  A success does not overwrite
     a login we already learned from /user. *)
  let on_api = function
    | Ok () -> (
        match !token_status with
        | Works _ -> ()
        | _ -> set_token_state (Works None))
    | Error e ->
        if Gh.is_auth_error e then
          set_token_state (Broken ("GitHub turned this token down: " ^ e))
  in

  (* --- input --- *)
  let input =
    El.textarea
      ~at:
        [
          cls "input";
          At.rows 4;
          At.placeholder
            (Jstr.v
               "Paste links, or a whole Slack message. Anything that is not a \
                pull request is ignored.");
        ]
      []
  in
  let rec add_ref ?(force = false) (r : Gh.pr_ref) =
    let key = Gh.ref_url r in
    if Urls.mem key !loaded && not force then ()
    else begin
      let slot =
        match Urls.find_opt key !loaded with
        (* A reload keeps the place the pull request already holds, rather
           than sending it to the bottom for as long as the fetch takes. *)
        | Some slot -> { slot with set_parent = ignore }
        | None ->
            {
              el = El.div ~at:[ cls "slot" ] [];
              order = Pr_order.pending r;
              set_parent = ignore;
            }
      in
      loaded := Urls.add key slot !loaded;
      El.set_children slot.el [ loading_card r ];
      refresh_list ();
      reorder ();
      incr pending;
      let settled () =
        decr pending;
        if !pending = 0 then say ""
      in
      (* A pull request dismissed while its fetch was in flight stays gone. *)
      let listed () = Urls.mem key !loaded in
      Fut.await (Gh.fetch_pr ~token r) (function
        | Ok pr ->
            settled ();
            on_api (Ok ());
            if listed () then begin
              let card, set_parent =
                render_pr ~token ~on_api ~on_gone:forget pr
              in
              El.set_children slot.el [ card ];
              loaded :=
                Urls.add key
                  { slot with order = order_of_pr pr; set_parent }
                  !loaded;
              reorder ()
            end
        | Error e ->
            settled ();
            on_api (Error e);
            if listed () then begin
              let retry = button "Retry" (fun () -> add_ref ~force:true r) in
              let drop = button "Remove" (fun () -> forget r) in
              El.set_children slot.el
                [
                  El.div
                    ~at:[ cls "pr failed" ]
                    [
                      El.div
                        ~at:[ cls "pr-line1" ]
                        [
                          El.span
                            ~at:[ cls "pr-num" ]
                            [ txt (Gh.ref_to_string r) ];
                        ];
                      El.div ~at:[ cls "error" ] [ txt e ];
                      El.div ~at:[ cls "actions" ] [ retry; drop ];
                    ];
                ]
            end)
    end
  in

  (* What clicking an entry does; set once the buttons exist, below. *)
  let replay = ref (fun (_ : entry) -> ()) in
  let rec render_history () =
    let entry_el e =
      let chips =
        List.map
          (fun r -> El.span ~at:[ cls "chip" ] [ txt (Gh.ref_to_string r) ])
          e.refs
      in
      let icon =
        match e.slack with
        | None -> []
        | Some _ ->
            let i = El.span ~at:[ cls "entry-icon" ] [] in
            set_svg i slack_svg;
            [ i ]
      in
      let src =
        match e.slack with
        | None -> excerpt e.source
        | Some o ->
            Printf.sprintf "#%s \xc2\xb7 %s: %s" o.channel o.author
              (excerpt e.source)
      in
      let hover =
        match e.slack with
        | None -> hover_text e.refs
        | Some o ->
            let what =
              if e.refs = [] then
                "No pull request in this message: click to open it in Slack."
              else hover_text e.refs
            in
            Printf.sprintf "%s\n\nIn #%s, from %s:\n%s" what o.channel o.author
              (Smashtb.Slack_text.excerpt 400 e.source)
      in
      (* The whole row is the button: the column is too narrow to spare width
         for a separate target. Everything sits on one line, cut off at the
         edge rather than wrapping, so every entry is the same height and the
         list stays scannable; the hover names the pull requests instead. *)
      let el =
        El.button
          ~at:[ cls "entry"; attr "type" "button"; At.title (Jstr.v hover) ]
          [
            El.span
              ~at:[ cls "entry-line" ]
              (icon @ chips @ [ El.span ~at:[ cls "entry-src" ] [ txt src ] ]);
          ]
      in
      on_click el (fun _ ->
          match (e.refs, e.slack) with
          | [], Some o when o.link <> "" ->
              ignore
                (Jv.call (Window.to_jv G.window) "open"
                   [| Jv.of_string o.link; Jv.of_string "_blank" |])
          | _ -> !replay e);
      el
    in
    let n = List.length !history in
    El.set_children hist_head
      (El.span
         ~at:[ cls "hist-title" ]
         [ txt (Printf.sprintf "History (%d)" n) ]
      ::
      (if n = 0 then []
       else
         [
           button ~classes:"btn tiny"
             ~title:"Forget every remembered paste and Slack message"
             "Clear history" (fun () ->
               history := [];
               save_history [];
               render_history ());
         ]));
    El.set_children history_el
      (if !history = [] then
         [
           El.div
             ~at:[ cls "hist-empty" ]
             [ txt "Pastes that yield a pull request are kept here." ];
         ]
       else List.map entry_el !history)
  in

  (* Remembering a paste rather than the pull request list: the list starts
     empty every session, and this is what puts a batch back. *)
  let remember ?entry source refs =
    let urls = List.map Gh.ref_url in
    let kept = List.filter (fun e -> urls e.refs <> urls refs) !history in
    (* An entry loaded again moves to the top as it was, keeping its own
       text and its Slack origin. *)
    let entry = Option.value entry ~default:{ source; refs; slack = None } in
    history := entry :: kept;
    history := keep_recent !history;
    save_history !history;
    render_history ()
  in

  let load ?entry () =
    let raw = Jstr.to_string (El.prop El.Prop.value input) in
    if token = "" then say ~bad:true no_token_msg
    else
      match Gh.extract_refs raw with
      | [] ->
          say ~bad:true
            "No pull request links in that text. Paste a github.com link, or \
             owner/repo#12."
      | refs ->
          let n = List.length refs in
          say
            (Printf.sprintf "Found %d %s\xe2\x80\xa6" n
               (plural n "pull request" "pull requests"));
          List.iter (fun r -> add_ref r) refs;
          remember ?entry raw refs
  in

  (* Ctrl+Enter (Cmd+Enter on a Mac) anywhere on the page presses Load, so a
     paste can be sent without reaching for the mouse. *)
  ignore
    (Ev.listen Ev.keydown
       (fun ev ->
         let k = Ev.as_type ev in
         if
           (Ev.Keyboard.ctrl_key k || Ev.Keyboard.meta_key k)
           && Jstr.equal (Ev.Keyboard.key k) (Jstr.v "Enter")
         then begin
           Ev.prevent_default ev;
           load ()
         end)
       (Document.as_target G.document));

  let set_all_prs b =
    ignore
      (El.fold_find_by_selector
         (fun el () -> set_open el b)
         (Jstr.v "details.pr") ())
  in

  let clear_list () =
    El.set_children list_el [];
    loaded := Urls.empty;
    refresh_list ()
  in
  let clear () =
    clear_list ();
    say "List cleared."
  in
  (* An entry plays out as if typed: Clear, the entry's pull requests in the
     box, one per line, then Load. *)
  (replay :=
     fun e ->
       clear ();
       El.set_prop El.Prop.value
         (Jstr.v (String.concat "\n" (List.map Gh.ref_url e.refs)))
         input;
       load ~entry:e ());

  let controls =
    El.div
      ~at:[ cls "controls" ]
      [
        button ~classes:"btn primary"
          ~title:"Load the pull requests found above (Ctrl+Enter)" "Load"
          (fun () -> load ());
        button ~classes:"btn danger"
          ~title:
            "Empty the pull request list, then load the pull requests found \
             above"
          "Clear & Load" (fun () ->
            clear_list ();
            load ());
        button "Expand all" (fun () -> set_all_prs true);
        button "Collapse all" (fun () -> set_all_prs false);
        button ~title:"Re-fetch every pull request currently shown" "Reload all"
          (fun () ->
            Urls.iter
              (fun k _ ->
                match Gh.parse_ref k with
                | Some r -> add_ref ~force:true r
                | None -> ())
              !loaded);
        button ~classes:"btn danger"
          ~title:
            "Remove every pull request from the list. Nothing is sent to \
             GitHub."
          "Clear \xf0\x9f\x91\x87" clear;
      ]
  in

  let header =
    El.header
      ~at:[ cls "top" ]
      [
        El.div
          ~at:[ cls "brand" ]
          [
            El.h1 [ txt "smashtb" ];
            El.span ~at:[ cls "tag" ] [ txt "review, approve, move on" ];
            El.div ~at:[ cls "badges" ] [ badge; slack_badge ];
          ];
        El.div
          ~at:[ cls "compose" ]
          [
            input;
            El.v
              ~at:[ cls "history-col" ]
              (Jstr.v "aside") [ hist_head; history_el ];
          ];
        controls;
        status;
      ]
  in
  let slack_me =
    if Slack.configured () then Slack.whoami ()
    else Fut.return (Error "Slack is off")
  in
  let slack_config = ref (Slack_config.load ()) in
  let slack_page = Slack_page.create ~me:slack_me ~config:slack_config in
  El.set_children doc_body [ header; empty_hint; list_el; slack_page.el ];
  track_height header;

  (* Redraws the Slack badge, once Slack is started below. *)
  let refresh_slack = ref ignore in
  (* The Slack page replaces the rest while the address ends in #slack... *)
  let route () =
    let hash =
      match Jstr.to_string (Uri.fragment (Window.location G.window)) with
      | "" -> ""
      | f -> "#" ^ f
    in
    let step =
      if Slack.configured () then Slack_page.step_of_hash hash else None
    in
    El.set_class (Jstr.v "on-slack") (Option.is_some step) doc_body;
    (* ...where the configuration may have changed. *)
    if Option.is_none step then !refresh_slack ();
    Option.iter
      (fun step ->
        slack_page.show step;
        ignore
          (Jv.call (Window.to_jv G.window) "scrollTo"
             [| Jv.of_int 0; Jv.of_int 0 |]))
      step
  in
  ignore
    (Ev.listen Ev.hashchange (fun _ -> route ()) (Window.as_target G.window));
  route ();
  render_history ();

  if token = "" then begin
    set_token_state (Broken no_token_msg);
    say ~bad:true no_token_msg
  end
  else begin
    (* Ask GitHub who the token belongs to, so the badge is settled before the
       first pull request is even loaded. *)
    set_token_state (Untested "Checking this token with GitHub\xe2\x80\xa6");
    say "";
    Fut.await (Gh.whoami ~token) (function
      | Ok login -> set_token_state (Works (Some login))
      | Error e ->
          set_token_state (Broken ("GitHub turned this token down: " ^ e)))
  end;

  (* A matching Slack message: say so, and keep it in the history so its pull
     requests can be loaded later. Names come from Slack, at most once each. *)
  let slack_names = ref Urls.empty in
  let name_of id =
    match Urls.find_opt id !slack_names with
    | Some n -> Fut.return n
    | None ->
        Fut.map
          (fun r ->
            let n = Result.value r ~default:id in
            slack_names := Urls.add id n !slack_names;
            n)
          (Slack.author_name id)
  in
  let on_slack_match (me : Slack.me option) (m : Slack.message) ~channel =
    let ids = m.user :: Smashtb.Slack_text.user_mentions m.text in
    Fut.await (Fut.of_list (List.map name_of ids)) @@ fun named ->
    let names = List.combine ids named in
    let author = List.assoc m.user names in
    let text =
      Smashtb.Slack_text.plain ~user:(fun id -> List.assoc_opt id names) m.text
    in
    let link =
      match me with
      | Some me when me.url <> "" ->
          Smashtb.Slack_text.permalink ~workspace:me.url ~channel:m.channel
            ~ts:m.ts ~thread_ts:m.thread_ts
      | _ -> ""
    in
    let refs = Gh.extract_refs m.text in
    let entry =
      { source = text; refs; slack = Some { channel; author; link } }
    in
    history :=
      entry
      :: List.filter
           (fun e ->
             match e.slack with Some o -> o.link <> link | None -> true)
           !history;
    history := keep_recent !history;
    save_history !history;
    render_history ();
    (* A click brings this tab forward (Notify does that) on the pull
       requests, with the message's own ones loaded. The message itself stays
       one click away, in the history. *)
    let show_here () =
      if El.class' (Jstr.v "on-slack") doc_body then
        Jv.set (Jv.get Jv.global "location") "hash" (Jv.of_string "");
      match refs with
      | [] ->
          say
            (Printf.sprintf
               "That message in #%s links no pull request; it is in the \
                history, which opens it in Slack."
               channel)
      | refs ->
          let n = List.length refs in
          List.iter (fun r -> add_ref r) refs;
          say
            (Printf.sprintf "Loaded %d %s from #%s." n
               (plural n "pull request" "pull requests")
               channel)
    in
    Notify.show ~title:("#" ^ channel)
      ~body:(author ^ ": " ^ Smashtb.Slack_text.excerpt 240 text)
      ~tag:m.channel ~on_click:show_here
  in
  refresh_slack :=
    start_slack ~me:slack_me ~config:slack_config ~on_match:on_slack_match
      set_slack_state slack_badge;

  (* The list deliberately starts empty; the history column is what brings a
     previous batch back. *)
  refresh_list ()
