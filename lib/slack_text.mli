(** Slack's message markup, read for people: what a notification or the History
    panel shows.

    In the text Slack sends, everything between [<] and [>] is markup: links
    ([<https://x|label>]), user mentions ([<@U123>]), channels ([<#C123|name>])
    and special mentions ([<!here>]). A literal [<], [>] or [&] arrives as
    [&lt;], [&gt;] or [&amp;]. See
    {{:https://docs.slack.dev/messaging/formatting-message-text}Slack's
     formatting guide}. *)

val plain : ?user:(string -> string option) -> string -> string
(** [plain ~user text] is [text] with its markup replaced by what it reads as: a
    link by its label, or its address when it has none ([mailto:] dropped); a
    user mention by [@] and the name [user] gives for the id, else its label,
    else the id; a channel by [#] and its name; [<!here>] and the like by
    [@here]; a user group by its name. Entities are decoded. A [<] with no
    closing [>] is kept as it is. [user] defaults to knowing no one. *)

val user_mentions : string -> string list
(** [user_mentions text] is the ids of the users [text] mentions without giving
    their names, in order of first appearance, each once: the ids worth looking
    up before calling {!plain}. *)

val excerpt : int -> string -> string
(** [excerpt max s] is [s] if it is at most [max] bytes long, else its first
    [max] bytes or fewer, cut at a UTF-8 character boundary, followed by an
    ellipsis. *)

val permalink :
  workspace:string ->
  channel:string ->
  ts:string ->
  thread_ts:string option ->
  string
(** [permalink ~workspace ~channel ~ts ~thread_ts] is the address of message
    [ts] in [channel], [workspace] being the workspace's address as [auth.test]
    gives it ([https://name.slack.com/], with or without the final slash). A
    reply in a thread ([thread_ts] set and different from [ts]) opens in its
    thread. *)
