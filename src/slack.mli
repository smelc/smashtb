(** Slack: the Web API, through run.sh's server, and Socket Mode.

    The page cannot call the Web API itself: slack.com does not let a browser
    send it an [Authorization] header, the only way some methods accept a token.
    So run.sh's server makes the calls, holding both tokens, and forwards the
    answers; the page never sees a token. The Socket Mode WebSocket is opened
    from the page, WebSockets not being subject to CORS.

    Every Web API call fails with Slack's error code, such as [invalid_auth] or
    [ratelimited], or with what went wrong reaching the local server. *)

val configured : unit -> bool
(** [configured ()] is whether run.sh found both Slack tokens. When it did not,
    nothing else here can work. *)

(** {1 Web API} *)

type me = {
  user : string;  (** the user's handle *)
  user_id : string;
  team : string;  (** the workspace's name *)
  team_id : string;
  url : string;  (** the workspace's address, like [https://name.slack.com/] *)
}
(** The user the token belongs to, and their workspace. *)

val whoami : unit -> me Task.t
(** [whoami ()] asks Slack who the user token belongs to. *)

type channel = {
  id : string;
  name : string;  (** without the leading [#] *)
  is_private : bool;
  is_shared : bool;
      (** shared with another organization, through Slack Connect *)
}

val channels : unit -> channel list Task.t
(** [channels ()] is the channels the user is a member of, archived ones left
    out, sorted by name. Those are the only channels Slack sends messages from.
*)

val author_name : string -> string Task.t
(** [author_name id] is how to show the author [id] of a message: a user by the
    display name they chose, else their real name, else their handle; a bot (an
    id starting with [B]) by its name. It falls back to [id] when Slack knows no
    better. *)

(** {1 Messages} *)

type message = {
  team : string;  (** the workspace's id *)
  channel : string;  (** the channel's id, not its name *)
  user : string;
      (** the author's id; for some bot messages the bot's name or id instead *)
  text : string;  (** with Slack's markup, see {!Smashtb.Slack_text} *)
  ts : string;  (** the message's id, unique within its channel *)
  thread_ts : string option;
      (** the id of the thread's first message, on replies in a thread and on
          the first message itself *)
  reply_count : int;
      (** how many replies the thread this message starts has; known only for
          messages read with {!history}, [0] otherwise *)
}

val is_reply : message -> bool
(** [is_reply m] is whether [m] is a reply in a thread, rather than a message of
    the channel itself. *)

val history : team:string -> n:int -> string -> message list Task.t
(** [history ~team ~n channel] is the messages among the last [n] of [channel],
    newest first. Replies in threads are not among them unless they were also
    sent to the channel; neither are edits, deletions, joins and the like, so
    there can be fewer than [n]. [team] is the workspace [channel] belongs to.
*)

val replies :
  team:string -> limit:int -> channel:string -> string -> message list Task.t
(** [replies ~team ~limit ~channel ts] is the replies in the thread started by
    message [ts] of [channel], oldest first, up to [limit] of them, without the
    first message itself. *)

(** {1 Socket Mode} *)

(** Where this tab's connection stands. *)
type state =
  | Other_tab  (** another tab has the connection; this one waits *)
  | Connecting
  | Connected
  | Retrying of { why : string; seconds : int }
      (** the connection failed or dropped, for reason [why]; the next try is in
          [seconds] *)

type t = {
  take_over : unit -> unit;
      (** [take_over ()] moves the connection to this tab, from whichever tab
          has it; that tab then waits. *)
}

val start : on_state:(state -> unit) -> on_message:(message -> unit) -> t
(** [start ~on_state ~on_message] connects, and stays connected for as long as
    the page lives, calling [on_state] on every change and [on_message] on every
    new message (edits, deletions and joins are left out).

    Slack sends each event to a single one of an app's connections, so only one
    tab of the page connects at a time: the one holding a browser lock. The
    others report {!Other_tab} and take over when that tab closes, or on
    {!t.take_over}. Every event is acknowledged on arrival. Slack ends
    connections every hour or so, warning first; the connection then moves to a
    new one at once. After a failure, the next try waits 1, 2, 4... seconds, up
    to a minute. In browsers without Web Locks, every tab connects. *)
