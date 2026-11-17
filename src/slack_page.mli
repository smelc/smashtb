(** The Slack settings page, in two steps: which channels to watch, then the
    pattern a message must match, tried out on recent messages of those
    channels.

    The page lives at [#slack] and [#slack/pattern], so a reload stays on it and
    the browser's Back button leaves it. Every change is saved as it is made:
    there is nothing to submit, and the page always shows the configuration in
    force. *)

type step =
  | Channels  (** the channels you are a member of, to tick *)
  | Pattern
      (** the pattern, with its matches in the last 16 messages of each watched
          channel and in the replies of their latest 8 threads *)

val step_of_hash : string -> step option
(** [step_of_hash h] is the step shown at address fragment [h] (such as
    ["#slack"]), if any. *)

val go_to : step -> unit
(** [go_to step] moves the address to [step]'s fragment. Showing it is up to
    whoever listens for address changes. *)

type t = {
  el : Brr.El.t;  (** the page, to put in the document once *)
  show : step -> unit;
      (** [show step] renders [step] afresh, asking Slack for what it shows *)
}

val create : me:Slack.me Task.t -> config:Slack_config.t ref -> t
(** [create ~me ~config] is the page. [me] is the workspace the user token
    belongs to; [config] the configuration, which the page reads and updates,
    saving every change. *)
