(** A minimal GitHub REST client, on the browser's fetch.

    api.github.com sends permissive CORS headers, so the page calls it directly
    with the token in an [Authorization] header. Every call fails with a message
    for people: GitHub's own message and the HTTP status when there is one. *)

(** {1 Pull request references} *)

type pr_ref = Smashtb.Pr_url.t = { owner : string; repo : string; number : int }

val ref_to_string : pr_ref -> string
(** See {!Smashtb.Pr_url.to_string}. *)

val ref_url : pr_ref -> string
(** See {!Smashtb.Pr_url.to_url}. *)

val parse_ref : string -> pr_ref option
(** See {!Smashtb.Pr_url.parse}. *)

val extract_refs : string -> pr_ref list
(** See {!Smashtb.Pr_url.extract}. *)

(** {1 Pull requests} *)

type file = {
  filename : string;
  previous_filename : string option;  (** set when the file was renamed *)
  status : string;
      (** [added], [removed], [modified], [renamed], [copied], [changed] or
          [unchanged] *)
  additions : int;
  deletions : int;
  patch : string option;
      (** the unified diff; absent for binary files and diffs GitHub deems too
          large *)
  blob_url : string;  (** the file at the pull request's head, on github.com *)
}
(** A file a pull request changes. *)

type pr = {
  pr_ref : pr_ref;
  title : string;
  author : string;  (** the author's login *)
  state : string;  (** [open] or [closed]; a merged pull request is closed *)
  draft : bool;
  merged : bool;
  html_url : string;
  base : string;  (** the branch it merges into *)
  head : string;  (** the branch it carries *)
  default_branch : string;
      (** the repository's default branch: a pull request whose [base] is that
          branch lands directly rather than onto another one *)
  additions : int;
  deletions : int;
  changed_files : int;
  files : file list;
  truncated : bool;
      (** [true] when the pull request has more files than were fetched *)
  approved_by : string list;
      (** logins whose latest verdict is an approval, see
          {!Smashtb.Reviews.approvers} *)
}

val fetch_pr : token:string -> pr_ref -> pr Task.t
(** [fetch_pr ~token r] is pull request [r] with its files, the first 1000 at
    most, and its approvals. That is several requests: the pull request, then
    its files and its reviews a hundred at a time. *)

val approve : token:string -> ?body:string -> pr_ref -> unit Task.t
(** [approve ~token ~body r] posts an approving review of [r] as the token's
    owner, with [body] as its text if non-empty (default: none). *)

val whoami : token:string -> string Task.t
(** [whoami ~token] is the login of the token's owner. *)

val is_auth_error : string -> bool
(** [is_auth_error msg] is [true] when [msg], an error from this module, says
    the token was refused (HTTP 401) or lacks a permission (HTTP 403), rather
    than something about a pull request. *)
