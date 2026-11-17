(** The browser's local storage, holding strings by key. *)

val get : string -> string option
(** [get key] is the value stored under [key], or [None] if nothing is. *)

val set : string -> string -> unit
(** [set key v] stores [v] under [key], replacing any previous value. Failures,
    such as a full store, are ignored. *)
