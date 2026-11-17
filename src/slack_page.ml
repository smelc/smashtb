(* The Slack settings page, in two steps: which channels to watch, then the
   pattern a message must match, tried out on the latest messages of those
   channels. It lives at #slack and #slack/pattern, so a reload stays on it and
   the browser's Back button leaves it.

   Every change is saved as it is made: there is nothing to submit, and the
   page always shows the configuration in force. *)

open Brr
open Ui
module Config = Slack_config
module Smap = Config.Smap

(* How many of each channel's latest messages the pattern is tried on. *)
let history_n = 16

(* Among those, how many threads have their replies read too, the latest
   first, and how many replies each. A busy channel can have a thread on most
   of its messages, and every thread costs a call to Slack. *)
let thread_cap = 8
let replies_cap = 200

(* The messages among [msgs] whose threads are read. *)
let threads_to_read (msgs : Slack.message list) =
  List.filteri
    (fun i _ -> i < thread_cap)
    (List.filter (fun (m : Slack.message) -> m.reply_count > 0) msgs)

type step = Channels | Pattern

let hash_of_step = function Channels -> "#slack" | Pattern -> "#slack/pattern"

let step_of_hash = function
  | "#slack" -> Some Channels
  | "#slack/pattern" -> Some Pattern
  | _ -> None

let go hash = Jv.set (Jv.get Jv.global "location") "hash" (Jv.of_string hash)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  n = 0 || at 0

let checkbox checked =
  let b = El.input ~at:[ attr "type" "checkbox" ] () in
  El.set_prop El.Prop.checked checked b;
  b

let when_changed el f =
  ignore (Ev.listen Ev.change (fun _ -> f ()) (El.as_target el))

let when_typed el f =
  ignore (Ev.listen Ev.input (fun _ -> f ()) (El.as_target el))

let value el = Jstr.to_string (El.prop El.Prop.value el)

let note ?(bad = false) msg =
  El.p ~at:[ cls (if bad then "sp-note bad" else "sp-note") ] [ txt msg ]

(* A message's time, as the browser writes dates where it is. *)
let sent_at ts =
  match float_of_string_opt ts with
  | None -> ts
  | Some secs ->
      let d =
        Jv.new' (Jv.get Jv.global "Date") [| Jv.of_float (secs *. 1000.) |]
      in
      Jstr.to_string (Jv.to_jstr (Jv.call d "toLocaleString" [||]))

type t = { el : El.t; show : step -> unit }

(* [me] is the workspace the user token belongs to. [config] is shared with
   the live listener, which reads it afresh for every message. *)
let create ~(me : Slack.me Gh.fut) ~config =
  let set c =
    config := c;
    Config.save c
  in
  let body = El.div ~at:[ cls "sp-body" ] [] in
  let el = El.div ~at:[ cls "slack-page" ] [ body ] in
  let nav buttons = El.div ~at:[ cls "sp-nav" ] buttons in

  (* {2 Step 1: channels} *)
  let channels_view () =
    let heading = El.h3 ~at:[ cls "sp-workspace" ] [] in
    let filter =
      El.input
        ~at:
          [
            cls "input sp-filter";
            attr "type" "search";
            attr "placeholder" "Filter channels";
          ]
        ()
    in
    let status = El.div [ note "Asking Slack for your channels\xe2\x80\xa6" ] in
    let list = El.div ~at:[ cls "sp-channels" ] [] in
    (* (name, is_private, is_shared, row), for filtering. *)
    let rows = ref [] in
    let hide_private = ref false and hide_shared = ref false in
    let apply_filters () =
      let f = String.lowercase_ascii (String.trim (value filter)) in
      List.iter
        (fun (name, priv, shared, row) ->
          let shown =
            contains ~sub:f (String.lowercase_ascii name)
            && (not (!hide_private && priv))
            && not (!hide_shared && shared)
          in
          El.set_class (Jstr.v "hidden") (not shown) row)
        !rows
    in
    when_typed filter apply_filters;
    (* A button that hides some channels while it is pressed. *)
    let toggle label title flag =
      let b = ref (El.div []) in
      b :=
        button ~classes:"btn tiny sp-toggle" ~title label (fun () ->
            flag := not !flag;
            El.set_class (Jstr.v "on") !flag !b;
            El.set_at (Jstr.v "aria-pressed")
              (Some (Jstr.v (string_of_bool !flag)))
              !b;
            apply_filters ());
      El.set_at (Jstr.v "aria-pressed") (Some (Jstr.v "false")) !b;
      !b
    in
    let filters =
      El.div
        ~at:[ cls "sp-filters" ]
        [
          filter;
          toggle "Hide private" "Leave private channels out of the list"
            hide_private;
          toggle "Hide shared"
            "Leave out channels shared with another organization" hide_shared;
        ]
    in
    El.set_children body
      [
        El.h2 [ txt "Slack \xc2\xb7 1. Channels to watch" ];
        note
          "Tick the channels whose new messages are checked against your \
           pattern. Only the channels you are a member of are listed: Slack \
           sends nothing from the others.";
        heading;
        filters;
        status;
        list;
        nav
          [
            button "Back to pull requests" (fun () -> go "");
            button ~classes:"btn primary" "Next: pattern \xe2\x86\x92"
              (fun () -> go (hash_of_step Pattern));
          ];
      ];
    let both =
      Fut.bind me @@ function
      | Error e -> Fut.return (Error e)
      | Ok me -> Fut.map (Result.map (fun cs -> (me, cs))) (Slack.channels ())
    in
    Fut.await both @@ function
    | Error e ->
        El.set_children status
          [ note ~bad:true ("Slack would not list your channels: " ^ e) ]
    | Ok ((me : Slack.me), chans) ->
        let team = me.team_id in
        let count = El.span ~at:[ cls "sp-count" ] [] in
        let update_count () =
          let n = List.length (Config.watched !config ~team) in
          El.set_children count
            [
              txt
                (Printf.sprintf "%d of %d %s watched" n (List.length chans)
                   (plural (List.length chans) "channel" "channels"));
            ]
        in
        El.set_children heading [ txt me.team; count ];
        update_count ();
        let row ?(gone = false) ?(priv = false) ?(shared = false)
            (ch : Config.channel) chips =
          let box = checkbox (Config.is_watched !config ~team ch.id) in
          when_changed box (fun () ->
              set
                (Config.set_watched !config ~team ch
                   (El.prop El.Prop.checked box));
              update_count ());
          let el =
            El.label
              ~at:[ cls (if gone then "sp-row gone" else "sp-row") ]
              (box :: El.span ~at:[ cls "sp-name" ] [ txt ch.name ] :: chips)
          in
          rows := (ch.name, priv, shared, el) :: !rows;
          el
        in
        let chip c label = El.span ~at:[ cls ("badge " ^ c) ] [ txt label ] in
        let live =
          List.map
            (fun (c : Slack.channel) ->
              row ~priv:c.is_private ~shared:c.is_shared
                { id = c.id; name = c.name }
                ((if c.is_private then [ chip "b-private" "private" ] else [])
                @ if c.is_shared then [ chip "b-shared" "shared" ] else []))
            chans
        in
        (* Watched channels Slack no longer lists: archived, or left. Shown so
           they can be unticked, rather than kept invisibly. *)
        let gone =
          List.filter
            (fun (w : Config.channel) ->
              not (List.exists (fun (c : Slack.channel) -> c.id = w.id) chans))
            (Config.watched !config ~team)
          |> List.map (fun w ->
              row ~gone:true w
                [ chip "b-gone" "no longer one of your channels" ])
        in
        El.set_children status
          (if chans = [] then [ note "You are not a member of any channel." ]
           else []);
        El.set_children list (gone @ live);
        apply_filters ()
  in

  (* {2 Step 2: pattern} *)
  let pattern_view () =
    let input =
      El.input
        ~at:
          [
            cls "input sp-pattern";
            attr "type" "text";
            attr "spellcheck" "false";
            attr "autocomplete" "off";
            attr "placeholder" "github\\.com/[\\w.-]+/[\\w.-]+/pull/\\d+";
          ]
        ()
    in
    El.set_prop El.Prop.value (Jstr.v !config.Config.pattern) input;
    let case = checkbox !config.ignore_case in
    let error = El.div [] in
    let results = El.div ~at:[ cls "sp-results" ] [] in
    (* By channel id: [None] while its history is on its way. *)
    let histories = ref Smap.empty in
    (* By channel id and thread: [None] while its replies are on their way. *)
    let threads = ref Smap.empty in
    let thread_key channel ts = channel ^ " " ^ ts in
    let watched = ref [] in
    (* User id -> name, and the ids already asked about. *)
    let names = ref Smap.empty in
    let asked = ref Smap.empty in
    let rec name_of (m : Slack.message) =
      match Smap.find_opt m.user !names with
      | Some n -> n
      | None ->
          let looks_like_id =
            String.length m.user > 1
            && (m.user.[0] = 'U' || m.user.[0] = 'W' || m.user.[0] = 'B')
          in
          if looks_like_id && not (Smap.mem m.user !asked) then begin
            asked := Smap.add m.user () !asked;
            Fut.await (Slack.author_name m.user) (function
              | Ok n ->
                  names := Smap.add m.user n !names;
                  render ()
              | Error _ -> ())
          end;
          m.user
    and message_el re (m : Slack.message) =
      let text =
        List.map
          (fun (s, hit) -> if hit then El.mark [ txt s ] else txt s)
          (Config.segments re m.text)
      in
      let reply = Slack.is_reply m in
      El.div
        ~at:[ cls (if reply then "sp-msg reply" else "sp-msg") ]
        [
          El.div
            ~at:[ cls "sp-meta" ]
            ((if reply then
                [
                  El.span
                    ~at:[ cls "sp-in-thread" ]
                    [ txt "\xe2\x86\xb3 in a thread" ];
                ]
              else [])
            @ [
                El.span ~at:[ cls "sp-who" ] [ txt (name_of m) ];
                El.span ~at:[ cls "sp-when" ] [ txt (sent_at m.ts) ];
              ]);
          El.div ~at:[ cls "sp-text" ] text;
        ]
    and render () =
      match Config.compile !config with
      | Error e ->
          El.set_children error
            [ note ~bad:true ("Not a valid regular expression: " ^ e) ]
      | Ok None ->
          El.set_children error [];
          El.set_children results
            [ note "Type a pattern to see which messages it matches." ]
      | Ok (Some re) ->
          El.set_children error [];
          let total = ref 0 in
          let block (ch : Config.channel) =
            let head extra =
              El.div
                ~at:[ cls "sp-chan-head" ]
                [ El.span ~at:[ cls "sp-name" ] [ txt ch.name ]; txt extra ]
            in
            match Smap.find_opt ch.id !histories with
            | None | Some None -> [ head " \xc2\xb7 loading\xe2\x80\xa6" ]
            | Some (Some (Error e)) -> [ head (" \xc2\xb7 Slack said: " ^ e) ]
            | Some (Some (Ok msgs)) ->
                let read = threads_to_read msgs in
                let thread_of (m : Slack.message) =
                  if List.memq m read then
                    Smap.find_opt (thread_key ch.id m.ts) !threads
                    |> Option.join
                  else None
                in
                (* A reply also sent to the channel is in both the history and
                   its thread: keep the first. *)
                let in_history (r : Slack.message) =
                  List.exists (fun (m : Slack.message) -> m.ts = r.ts) msgs
                in
                let replies (m : Slack.message) =
                  match thread_of m with
                  | Some (Ok rs) -> List.filter (fun r -> not (in_history r)) rs
                  | _ -> []
                in
                let all = List.concat_map (fun m -> m :: replies m) msgs in
                let n_replies = List.length all - List.length msgs in
                let hits =
                  List.filter
                    (fun (m : Slack.message) -> Config.test re m.text)
                    all
                in
                total := !total + List.length hits;
                let pending =
                  List.length (List.filter (fun m -> thread_of m = None) read)
                in
                let failed =
                  List.length
                    (List.filter
                       (fun m ->
                         match thread_of m with
                         | Some (Error _) -> true
                         | _ -> false)
                       read)
                in
                let threads_note =
                  let n_threads =
                    List.length
                      (List.filter
                         (fun (m : Slack.message) -> m.reply_count > 0)
                         msgs)
                  in
                  if read = [] then ""
                  else if pending > 0 then
                    Printf.sprintf ", reading %d %s\xe2\x80\xa6" pending
                      (plural pending "thread" "threads")
                  else
                    Printf.sprintf ", plus %d %s in %s%s" n_replies
                      (plural n_replies "reply" "replies")
                      (if n_threads > List.length read then
                         Printf.sprintf "the latest %d of %d threads"
                           (List.length read) n_threads
                       else
                         Printf.sprintf "%d %s" (List.length read)
                           (plural (List.length read) "thread" "threads"))
                      (if failed > 0 then
                         Printf.sprintf " (%d could not be read)" failed
                       else "")
                in
                head
                  (Printf.sprintf " \xc2\xb7 %d %s in the last %d %s%s"
                     (List.length hits)
                     (plural (List.length hits) "match" "matches")
                     (List.length msgs)
                     (plural (List.length msgs) "message" "messages")
                     threads_note)
                :: List.map (message_el re) hits
          in
          let blocks =
            List.map
              (fun ch -> El.div ~at:[ cls "sp-chan" ] (block ch))
              !watched
          in
          El.set_children results
            (note
               (Printf.sprintf "%d matching %s." !total
                  (plural !total "message" "messages"))
            :: blocks)
    in
    when_typed input (fun () ->
        set { !config with pattern = value input };
        render ());
    when_changed case (fun () ->
        set { !config with ignore_case = El.prop El.Prop.checked case };
        render ());
    El.set_children body
      [
        El.h2 [ txt "Slack \xc2\xb7 2. Pattern" ];
        note
          "A message is picked up when this regular expression (JavaScript \
           syntax) matches somewhere in its text. The text is as Slack sends \
           it: links look like <https://example.com|label>, mentions like \
           <@U012AB3CD>, and &, < and > are written &amp;, &lt; and &gt;.";
        El.div
          ~at:[ cls "sp-pattern-row" ]
          [ input; El.label ~at:[ cls "sp-case" ] [ case; txt "Ignore case" ] ];
        error;
        El.h3
          [
            txt
              (Printf.sprintf
                 "Matches in the last %d messages of each watched channel"
                 history_n);
          ];
        note
          (Printf.sprintf
             "Replies in threads are included, for the latest %d threads of \
              each channel."
             thread_cap);
        results;
        nav
          [
            button "\xe2\x86\x90 Channels" (fun () ->
                go (hash_of_step Channels));
            button ~classes:"btn primary"
              "Next: back to pull requests \xe2\x86\x92" (fun () -> go "");
          ];
      ];
    El.set_children results [ note "Fetching the latest messages\xe2\x80\xa6" ];
    Fut.await me @@ function
    | Error e ->
        El.set_children results
          [ note ~bad:true ("Slack turned the user token down: " ^ e) ]
    | Ok me ->
        let team = me.team_id in
        watched := Config.watched !config ~team;
        if !watched = [] then
          El.set_children results
            [ note "No channel is ticked yet: go back a step and tick some." ]
        else begin
          List.iter
            (fun (ch : Config.channel) ->
              histories := Smap.add ch.id None !histories;
              Fut.await (Slack.history ~team ~n:history_n ch.id) (fun r ->
                  histories := Smap.add ch.id (Some r) !histories;
                  (match r with
                  | Error _ -> ()
                  | Ok msgs ->
                      List.iter
                        (fun (m : Slack.message) ->
                          let key = thread_key ch.id m.ts in
                          threads := Smap.add key None !threads;
                          Fut.await
                            (Slack.replies ~team ~limit:replies_cap
                               ~channel:ch.id m.ts) (fun r ->
                              threads := Smap.add key (Some r) !threads;
                              render ()))
                        (threads_to_read msgs));
                  render ()))
            !watched;
          render ()
        end
  in
  let show = function
    | Channels -> channels_view ()
    | Pattern -> pattern_view ()
  in
  { el; show }
