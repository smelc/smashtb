(** Who currently approves a pull request, from its reviews.

    GitHub keeps every review, so a reviewer who approved and later asked for
    changes still has an approval in the list. Only a verdict (an approval, a
    request for changes, or a dismissal) supersedes an earlier one: a plain
    comment leaves the reviewer's stance as it was. *)

val approvers : (string * string) list -> string list
(** [approvers reviews] is the logins whose latest verdict is an approval,
    sorted. [reviews] are [(login, state)] pairs, oldest first, [state] being
    GitHub's: [APPROVED], [CHANGES_REQUESTED], [DISMISSED], [COMMENTED] or
    [PENDING]. *)
