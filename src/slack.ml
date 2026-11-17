(* Slack, over Socket Mode.

   The Web API cannot be called from the page: slack.com answers the CORS
   preflight without allowing an Authorization header, and that header is the
   only way apps.connections.open accepts a token. So run.sh's server makes
   those calls, holding both tokens, and the page only ever sees the answers.
   The WebSocket that Socket Mode hands back is opened from here: WebSockets are
   not subject to CORS.

   Slack sends each event to a single one of an app's connections, so a second
   tab connecting would quietly steal half the messages from the first. Only
   the tab holding a Web Lock connects; the others wait for it to go away. *)

open Brr
open Brr_io
module J = Gh.J

let ( let* ) = Gh.( let* )
let return = Gh.return

(* [true] when run.sh found both tokens. The page never sees the tokens
   themselves. *)
let configured () =
  let v = Jv.get Jv.global "SMASHTB_SLACK" in
  (not (Jv.is_none v)) && Jv.to_bool v

(* {1 Web API, through run.sh} *)

(* The custom header is what keeps other web sites out: they cannot set it on a
   request to localhost without a preflight, and run.sh answers none. *)
let post path params : Jv.t Gh.fut =
  let headers =
    Fetch.Headers.of_assoc
      [
        (Jstr.v "Content-Type", Jstr.v "application/x-www-form-urlencoded");
        (Jstr.v "X-Smashtb", Jstr.v "1");
      ]
  in
  let body =
    Fetch.Body.of_jstr
      (Uri.Params.to_jstr
         (Uri.Params.of_assoc
            (List.map (fun (k, v) -> (Jstr.v k, Jstr.v v)) params)))
  in
  let init = Fetch.Request.init ~method':(Jstr.v "POST") ~headers ~body () in
  Fut.bind (Fetch.url ~init (Jstr.v path)) @@ function
  | Error e -> Fut.return (Error (Gh.err_msg e))
  | Ok resp -> (
      let status = Fetch.Response.status resp in
      Fut.bind (Fetch.Body.text (Fetch.Response.as_body resp)) @@ function
      | Error e -> Fut.return (Error (Gh.err_msg e))
      | Ok text -> (
          match Json.decode text with
          | Error _ ->
              Fut.return
                (Error
                   (Printf.sprintf "HTTP %d from the local server: %s" status
                      (Jstr.to_string text)))
          | Ok j ->
              if J.bool j "ok" then return j
              else Fut.return (Error (J.str ~default:"unknown error" j "error"))
          ))

let call meth params = post ("/slack/api/" ^ meth) params

(* The handle of the user the token belongs to. *)
let whoami () =
  let* j = call "auth.test" [] in
  return (J.str j "user")

let connection_url () =
  let* j = post "/slack/connect" [] in
  return (J.str j "url")

(* {1 Messages} *)

type message = {
  channel : string;  (** the channel's id, not its name *)
  user : string;  (** a user id, or the bot's id or name for bot messages *)
  text : string;
  ts : string;  (** unique within the channel *)
  thread_ts : string option;  (** set on replies in a thread *)
}

(* A message someone wrote. The other subtypes are edits, deletions, joins and
   the like, none of which is a new message. *)
let message_of_event ev =
  let is_new =
    match J.str_opt ev "subtype" with
    | None | Some ("bot_message" | "thread_broadcast" | "file_share") -> true
    | Some _ -> false
  in
  if J.str ev "type" <> "message" || not is_new then None
  else
    let user =
      List.find_opt
        (fun s -> s <> "")
        [ J.str ev "user"; J.str ev "username"; J.str ev "bot_id" ]
    in
    Some
      {
        channel = J.str ev "channel";
        user = Option.value user ~default:"";
        text = J.str ev "text";
        ts = J.str ev "ts";
        thread_ts = J.str_opt ev "thread_ts";
      }

(* {1 Connection} *)

type state =
  | Other_tab  (** another tab has the connection *)
  | Connecting
  | Connected
  | Retrying of { why : string; seconds : int }

(* 1, 2, 4... seconds, capped at a minute. *)
let backoff attempt = min 60 (1 lsl min attempt 6)

let ack ws envelope_id =
  let j = Jv.obj [| ("envelope_id", Jv.of_string envelope_id) |] in
  Websocket.send_string ws (Json.encode j)

type t = { take_over : unit -> unit }
(** [take_over ()] moves the connection to this tab, from whichever tab has it.
*)

(* A Promise that never settles: returned from a Web Lock callback, it keeps
   the lock for as long as the page lives, or until another tab steals it. *)
let forever () =
  Jv.new'
    (Jv.get Jv.global "Promise")
    [| Jv.callback ~arity:2 (fun _ _ -> ()) |]

(* Connect and stay connected: Slack drops every connection after an hour or
   so, warning first, and network trouble ends one at any time. Only while this
   tab holds the lock, though. *)
let start ~on_state ~on_message =
  let attempt = ref 0 in
  (* Bumped whenever this tab gains or loses the lock. Sockets, retries and
     answers belonging to an earlier run see it has moved on and do nothing. *)
  let run = ref 0 in
  let socket = ref None in
  let rec connect r () =
    if r = !run then begin
      on_state Connecting;
      Fut.await (connection_url ()) @@ function
      | _ when r <> !run -> ()
      | Error e -> retry r e
      | Ok url ->
          let ws = Websocket.create (Jstr.v url) in
          socket := Some ws;
          (* Set once this socket's end has been dealt with, so the close
             event of a socket we replaced, or that already failed, does
             nothing. *)
          let done_ = ref false in
          let finish k =
            if r = !run && not !done_ then begin
              done_ := true;
              k ()
            end
          in
          let on_envelope j =
            (match J.str_opt j "envelope_id" with
            | Some id -> ack ws id
            | None -> ());
            match J.str j "type" with
            | "hello" ->
                attempt := 0;
                on_state Connected
            | "disconnect" ->
                (* A warning, a refresh request, or Socket Mode being switched
                   off: in every case this socket is about to go, so move to a
                   new one now rather than waiting for it to drop. *)
                finish (fun () ->
                    Websocket.close ws;
                    if J.str j "reason" = "link_disabled" then
                      retry r "Socket Mode is turned off for this Slack app"
                    else connect r ())
            | "events_api" -> (
                let ev = Jv.get (Jv.get j "payload") "event" in
                if not (Jv.is_none ev) then
                  match message_of_event ev with
                  | Some m -> on_message m
                  | None -> ())
            | _ -> ()
          in
          let target = Websocket.as_target ws in
          ignore
            (Ev.listen Message.Ev.message
               (fun ev ->
                 let data : Jstr.t = Message.Ev.data (Ev.as_type ev) in
                 match Json.decode data with
                 | Ok j when r = !run -> on_envelope j
                 | _ -> ())
               target);
          ignore
            (Ev.listen Websocket.Ev.close
               (fun ev ->
                 let c = Ev.as_type ev in
                 finish (fun () ->
                     retry r
                       (Printf.sprintf "connection closed (code %d)"
                          (Websocket.Ev.Close.code c))))
               target)
    end
  and retry r why =
    if r = !run then begin
      let seconds = backoff !attempt in
      incr attempt;
      on_state (Retrying { why; seconds });
      ignore (G.set_timeout ~ms:(seconds * 1000) (connect r))
    end
  in
  let begin_run () =
    incr run;
    attempt := 0;
    connect !run ()
  in
  let end_run () =
    incr run;
    Option.iter (fun ws -> Websocket.close ws) !socket;
    socket := None;
    on_state Other_tab
  in
  let locks = Jv.get (Navigator.to_jv G.navigator) "locks" in
  if Jv.is_none locks then begin
    (* No Web Locks: every tab connects, and none is ever told to wait. *)
    begin_run ();
    { take_over = ignore }
  end
  else
    let name = Jv.of_string "smashtb-slack" in
    let holding _lock =
      begin_run ();
      forever ()
    in
    (* Whether this tab already has a request waiting for the lock: one is
       enough, however many times the lock is stolen from it. *)
    let queued = ref false in
    (* [p] is the promise of a lock request. It rejects if the lock it got is
       stolen, and this tab then waits for its turn again. *)
    let rec watch p =
      ignore (Jv.call p "catch" [| Jv.callback ~arity:1 (fun _ -> lost ()) |])
    and lost () =
      end_run ();
      wait ()
    and wait () =
      if not !queued then begin
        queued := true;
        watch
          (Jv.call locks "request"
             [|
               name;
               Jv.callback ~arity:1 (fun lock ->
                   queued := false;
                   holding lock);
             |])
      end
    in
    let request opts f =
      watch (Jv.call locks "request" [| name; opts; Jv.callback ~arity:1 f |])
    in
    request
      (Jv.obj [| ("ifAvailable", Jv.true') |])
      (fun lock ->
        if Jv.is_null lock then begin
          on_state Other_tab;
          wait ();
          Jv.undefined
        end
        else holding lock);
    {
      take_over = (fun () -> request (Jv.obj [| ("steal", Jv.true') |]) holding);
    }
