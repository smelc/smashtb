(** Parsing the unified-diff patches the GitHub API returns, one per file.

    A patch is a series of hunks, each opened by a header such as
    [@@ -12,7 +12,9 @@ let f x =] and followed by context, removed and added
    lines. Parsing numbers every line, which is all the diff view needs. *)

(** What a line of a patch is. *)
type kind =
  | Context  (** unchanged, shown for context *)
  | Added
  | Removed
  | Hunk  (** a [@@ ... @@] header *)
  | Meta  (** anything else, such as [\ No newline at end of file] *)

type line = {
  kind : kind;
  old_no : int option;
      (** the line's number in the old file; [None] for added lines, hunk
          headers and meta lines *)
  new_no : int option;
      (** the line's number in the new file; [None] for removed lines, hunk
          headers and meta lines *)
  text : string;
      (** the line without its leading [+], [-] or space; hunk headers and meta
          lines are kept whole *)
}

val parse : string -> line list
(** [parse patch] is every line of [patch], in order. It never fails: a hunk
    header it cannot read keeps the numbering going from the previous hunk, an
    empty line counts as an empty context line, and a trailing newline is
    ignored. *)
