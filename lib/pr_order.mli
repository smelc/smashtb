(** Ordering the pull requests in the list: the ones closest to landing first.

    A pull request that targets its repository's default branch lands next, so
    it comes first. One that targets the branch of another loaded pull request
    lands only after it, so it comes one step further down, and so on up the
    stack: a stack reads top to bottom in the order it will merge. Pull requests
    at the same depth go by number, smallest first.

    Depth is read off the pull requests given, because that is all there is to
    go on. A pull request whose base is neither the default branch nor the
    branch of one of them has no known place in a stack, and goes last; so does
    one in a cycle of bases. *)

type t = {
  owner : string;
  repo : string;
  number : int;
  base : string;  (** the branch it merges into; [""] while unknown *)
  head : string;  (** the branch it carries; [""] while unknown *)
  base_is_default : bool;
      (** whether [base] is the repository's default branch *)
}
(** What ordering needs to know about a pull request. *)

val pending : Pr_url.t -> t
(** [pending r] is what is known of [r] before GitHub has answered: enough to
    break ties by number, not enough to place it in a stack, so it sorts last.
*)

val parent : t list -> t -> t option
(** [parent prs t] is the pull request among [prs] whose branch [t] merges into,
    when [t] is stacked on one rather than aimed at the default branch. Branches
    are matched within a repository only. When several pull requests carry that
    branch, the smallest number wins. *)

val sort : ('a -> t) -> 'a list -> 'a list
(** [sort key items] is [items] in merge order, [key] giving each one's pull
    request. Items in the same place keep their relative order. *)
