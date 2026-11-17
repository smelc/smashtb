(** Recognising the pull request references people paste. *)

type t = { owner : string; repo : string; number : int }
(** A pull request, by repository and number. *)

val to_string : t -> string
(** [to_string r] is the short form, [owner/repo#12]. *)

val to_url : t -> string
(** [to_url r] is the pull request's page on github.com. *)

val parse : string -> t option
(** [parse s] reads one reference, ignoring surrounding whitespace. It accepts
    - [https://github.com/owner/repo/pull/12], with or without the scheme, and
      with any suffix such as [/files], a query string or a [#fragment];
    - [https://api.github.com/repos/owner/repo/pulls/12];
    - [owner/repo#12].

    Anything else, including issues and numbers that are not positive, is
    [None]. *)

val extract : string -> t list
(** [extract text] is every distinct pull request referenced anywhere in [text],
    in order of first appearance. Everything else is ignored, so prose works: a
    whole Slack message, with its [<link|label>] markup, links wrapped in
    markdown, or sentence punctuation against a link. *)
