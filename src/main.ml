(* smashtb: paste a list of GitHub pull request links, read the diffs, approve
   or dismiss.  Approving posts a review through the API; dismissing only drops
   the pull request from this page. *)

open Brr
module Diff = Smashtb.Diff

let cls c = At.class' (Jstr.v c)
let txt = El.txt'
let attr n v = At.v (Jstr.v n) (Jstr.v v)

let on_click el f =
  ignore (Ev.listen Ev.click (fun ev -> f ev) (El.as_target el))

let set_disabled el b =
  El.set_at (Jstr.v "disabled") (if b then Some Jstr.empty else None) el

let set_open el b =
  El.set_at (Jstr.v "open") (if b then Some Jstr.empty else None) el

let details ?(at = []) children = El.v ~at (Jstr.v "details") children
let summary ?(at = []) children = El.v ~at (Jstr.v "summary") children

let button ?(classes = "btn") ?title label f =
  let at = [ cls classes; attr "type" "button" ] in
  let at =
    match title with None -> at | Some t -> At.title (Jstr.v t) :: at
  in
  let b = El.button ~at [ txt label ] in
  on_click b (fun ev ->
      Ev.stop_propagation ev;
      Ev.prevent_default ev;
      f ());
  b

let plural n one many = if n = 1 then one else many

let no_token_msg =
  "No GITHUB_TOKEN. Start the app with ./run.sh so a token is passed in."

module Urls = Map.Make (String)

(* {1 Persistence} *)

let store = Brr_io.Storage.local G.window

let store_get key =
  match Brr_io.Storage.get_item store (Jstr.v key) with
  | None -> ""
  | Some v -> Jstr.to_string v

let store_set key v =
  ignore (Brr_io.Storage.set_item store (Jstr.v key) (Jstr.v v))

let history_key = "smashtb.history"
let history_limit = 16

(* {1 History}

   The pull request list itself is deliberately not restored: a new session
   starts empty. What is kept is the last few pastes that yielded something, so
   a batch can be put back with one click. *)

type entry = {
  source : string;
      (** the text that was pasted, kept for recognising the entry *)
  refs : Gh.pr_ref list;  (** what was extracted from it *)
}

let entry_to_jv e =
  Jv.obj
    [|
      ("source", Jv.of_string e.source);
      ("refs", Jv.of_list Jv.of_string (List.map Gh.ref_url e.refs));
    |]

let entry_of_jv j =
  let str v = if Jv.is_none v then "" else Jstr.to_string (Jv.to_jstr v) in
  let refs = Jv.get j "refs" in
  {
    source = str (Jv.get j "source");
    refs =
      (if Jv.is_none refs then []
       else
         List.filter_map
           (fun v -> Gh.parse_ref (str v))
           (Jv.to_list Fun.id refs));
  }

let load_history () =
  match Json.decode (Jstr.v (store_get history_key)) with
  | Error _ -> []
  | Ok j ->
      if Jv.is_none j then []
      else List.filter (fun e -> e.refs <> []) (Jv.to_list entry_of_jv j)

let save_history entries =
  store_set history_key
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

let token_badge () =
  let label = El.span ~at:[ cls "tok-label" ] [ txt "token" ] in
  let glyph = El.span ~at:[ cls "tok-glyph" ] [] in
  let el = El.div ~at:[ cls "token-badge" ] [ label; glyph ] in
  let state = ref (Untested "") in
  let set s =
    state := s;
    let mark, style, tip =
      match s with
      | Untested why -> ("\xe2\x9d\x93", "tok-untested", why)
      | Works None ->
          ("\xe2\x9c\x85", "tok-works", "GitHub accepted this token.")
      | Works (Some login) ->
          ( "\xe2\x9c\x85",
            "tok-works",
            Printf.sprintf
              "GitHub accepted this token. Approvals are posted as %s." login )
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

let icon ~kind ~tip markup =
  let el =
    El.span ~at:[ cls ("pr-status " ^ kind); At.title (Jstr.v tip) ] []
  in
  Jv.set (El.to_jv el) "innerHTML" (Jv.of_string markup);
  el

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
                txt
                  (Printf.sprintf "by %s \xc2\xb7 %s \xe2\x86\x90 %s \xc2\xb7 "
                     pr.author pr.base pr.head);
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
  card

(* {1 Shell} *)

let loading_card (r : Gh.pr_ref) =
  El.div
    ~at:[ cls "pr placeholder" ]
    [ txt (Printf.sprintf "Loading %s\xe2\x80\xa6" (Gh.ref_to_string r)) ]

let () =
  let doc_body = Document.body G.document in

  (* --- state --- *)
  let token = token_from_page () in
  let badge, set_token_state, token_status = token_badge () in
  (* url -> the slot showing that pull request, so a card can be found again to
     replace or remove it. *)
  let loaded = ref Urls.empty in
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

  let forget r =
    loaded := Urls.remove (Gh.ref_url r) !loaded;
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
        | Some slot -> slot
        | None ->
            let slot = El.div ~at:[ cls "slot" ] [] in
            loaded := Urls.add key slot !loaded;
            El.append_children list_el [ slot ];
            slot
      in
      El.set_children slot [ loading_card r ];
      refresh_list ();
      incr pending;
      let settled () =
        decr pending;
        if !pending = 0 then say ""
      in
      Fut.await (Gh.fetch_pr ~token r) (function
        | Ok pr ->
            settled ();
            on_api (Ok ());
            El.set_children slot [ render_pr ~token ~on_api ~on_gone:forget pr ]
        | Error e ->
            settled ();
            on_api (Error e);
            let retry = button "Retry" (fun () -> add_ref ~force:true r) in
            let drop =
              button "Remove" (fun () ->
                  El.remove slot;
                  forget r)
            in
            El.set_children slot
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
              ])
    end
  in

  let rec render_history () =
    let entry_el e =
      let n = List.length e.refs in
      let chips =
        List.map
          (fun r -> El.span ~at:[ cls "chip" ] [ txt (Gh.ref_to_string r) ])
          e.refs
      in
      (* The whole row is the button: the column is too narrow to spare width
         for a separate target. Everything sits on one line, cut off at the
         edge rather than wrapping, so every entry is the same height and the
         list stays scannable; the hover names the pull requests instead. *)
      let el =
        El.button
          ~at:
            [
              cls "entry";
              attr "type" "button";
              At.title (Jstr.v (hover_text e.refs));
            ]
          [
            El.span
              ~at:[ cls "entry-line" ]
              (chips
              @ [ El.span ~at:[ cls "entry-src" ] [ txt (excerpt e.source) ] ]);
          ]
      in
      on_click el (fun _ ->
          List.iter (fun r -> add_ref r) e.refs;
          say
            (Printf.sprintf "Loaded %d %s from history." n
               (plural n "pull request" "pull requests")));
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
           button ~classes:"btn tiny" ~title:"Forget every remembered paste"
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
  let remember source refs =
    let urls = List.map Gh.ref_url in
    let kept = List.filter (fun e -> urls e.refs <> urls refs) !history in
    history := { source; refs } :: kept;
    if List.length !history > history_limit then
      history := List.filteri (fun i _ -> i < history_limit) !history;
    save_history !history;
    render_history ()
  in

  let load () =
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
          remember raw refs;
          El.set_prop El.Prop.value Jstr.empty input
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

  let controls =
    El.div
      ~at:[ cls "controls" ]
      [
        button ~classes:"btn primary"
          ~title:"Load the pull requests found above (Ctrl+Enter)" "Load" load;
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
        button
          ~title:
            "Remove every pull request from the list. Nothing is sent to \
             GitHub."
          "Clear" (fun () ->
            El.set_children list_el [];
            loaded := Urls.empty;
            refresh_list ();
            say "List cleared.");
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
            badge;
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
  El.set_children doc_body [ header; empty_hint; list_el ];
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

  (* The list deliberately starts empty; the history column is what brings a
     previous batch back. *)
  refresh_list ()
