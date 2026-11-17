(** What to listen to on Slack: the channels to watch, by workspace, and the
    pattern a message must match.

    Kept in the browser's local storage, so a restart needs no reconfiguring.
    Values are immutable: a change makes a new configuration, which {!save}
    stores. *)

type channel = {
  id : string;
  name : string;
      (** remembered so the channel can still be shown once Slack stops listing
          it *)
}
(** A watched channel. *)

type t
(** A configuration. *)

(** {1 Storage} *)

val load : unit -> t
(** [load ()] is the saved configuration; with nothing saved, or something
    unreadable, no channel, no pattern, and case ignored. *)

val save : t -> unit
(** [save c] stores [c] for {!load} to find, replacing what was there. *)

(** {1 Channels} *)

val watched : t -> team:string -> channel list
(** [watched c ~team] is the channels watched in workspace [team] (an id),
    sorted by name. *)

val watched_anywhere : t -> channel list
(** [watched_anywhere c] is every watched channel, whatever its workspace. *)

val is_watched : t -> team:string -> string -> bool
(** [is_watched c ~team id] is whether channel [id] of workspace [team] is
    watched. *)

val set_watched : t -> team:string -> channel -> bool -> t
(** [set_watched c ~team ch on] is [c] with [ch], in workspace [team], watched
    if [on] and not otherwise. *)

(** {1 Pattern} *)

val pattern : t -> string
(** [pattern c] is the pattern, a JavaScript regular expression as typed; [""]
    when none is set yet. *)

val ignore_case : t -> bool
(** [ignore_case c] is whether the pattern ignores case. *)

val with_pattern : t -> string -> t
(** [with_pattern c p] is [c] with pattern [p]. *)

val with_ignore_case : t -> bool -> t
(** [with_ignore_case c b] is [c] ignoring case if [b]. *)

type regex
(** A compiled pattern. *)

val compile : t -> (regex option, string) result
(** [compile c] is [c]'s pattern compiled: [Ok None] when there is none, which
    matches nothing; [Error msg] with the browser's own complaint when it is not
    a valid regular expression. *)

val test : regex -> string -> bool
(** [test re text] is whether [re] matches somewhere in [text]. *)

val matches : t -> string -> bool
(** [matches c text] is whether [c]'s pattern matches somewhere in [text];
    [false] when there is no pattern or it is invalid. *)

val segments : regex -> string -> (string * bool) list
(** [segments re text] is [text] cut into consecutive pieces, each paired with
    whether it is a match of [re], for highlighting. Concatenated, the pieces
    give back [text]. Empty matches are skipped. *)
