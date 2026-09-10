(* Minimal GitHub REST client, built on the browser's fetch.

   api.github.com sends permissive CORS headers, so the page can talk to it
   directly with an "Authorization: Bearer <token>" header; no proxy needed. *)

open Brr
open Brr_io

type 'a fut = ('a, string) result Fut.t

let ( let* ) x f =
  Fut.bind x (function Error e -> Fut.return (Error e) | Ok v -> f v)

let return v = Fut.return (Ok v)

(* {1 Reading JSON without a codec library} *)

module J = struct
  let str ?(default = "") j n =
    let v = Jv.get j n in
    if Jv.is_none v then default else Jstr.to_string (Jv.to_jstr v)

  let str_opt j n =
    let v = Jv.get j n in
    if Jv.is_none v then None else Some (Jstr.to_string (Jv.to_jstr v))

  let int ?(default = 0) j n =
    let v = Jv.get j n in
    if Jv.is_none v then default else Jv.to_int v

  let bool j n =
    let v = Jv.get j n in
    if Jv.is_none v then false else Jv.to_bool v

  let list f j = if Jv.is_none j then [] else Jv.to_list f j
end

(* {1 Pull request references} *)

(* The parsing itself lives in the browser-free library, so it can be tested. *)
type pr_ref = Smashtb.Pr_url.t = { owner : string; repo : string; number : int }

let ref_to_string = Smashtb.Pr_url.to_string
let ref_url = Smashtb.Pr_url.to_url
let parse_ref = Smashtb.Pr_url.parse
let extract_refs = Smashtb.Pr_url.extract

(* {1 HTTP} *)

let api = "https://api.github.com"

let headers token =
  Fetch.Headers.of_assoc
    [
      (Jstr.v "Authorization", Jstr.v ("Bearer " ^ token));
      (Jstr.v "Accept", Jstr.v "application/vnd.github+json");
      (Jstr.v "X-GitHub-Api-Version", Jstr.v "2022-11-28");
    ]

let err_msg e = Jstr.to_string (Jv.Error.message e)

(* GitHub error bodies look like {"message": "...", "errors": [...]}.  Surface
   the message when there is one, the raw status otherwise. *)
let error_of_body ~status (body : string) =
  let fallback = Printf.sprintf "HTTP %d" status in
  match Json.decode (Jstr.v body) with
  | Error _ ->
      if body = "" then fallback else Printf.sprintf "%s: %s" fallback body
  | Ok j -> (
      match J.str_opt j "message" with
      | None -> fallback
      | Some m -> Printf.sprintf "%s (HTTP %d)" m status)

let request ~token ~meth ?body url : Jv.t fut =
  let body = Option.map (fun b -> Fetch.Body.of_jstr (Jstr.v b)) body in
  let init =
    Fetch.Request.init ~method':(Jstr.v meth) ~headers:(headers token) ?body ()
  in
  Fut.bind (Fetch.url ~init (Jstr.v url)) @@ function
  | Error e -> Fut.return (Error (err_msg e))
  | Ok resp -> (
      let status = Fetch.Response.status resp in
      Fut.bind (Fetch.Body.text (Fetch.Response.as_body resp)) @@ function
      | Error e -> Fut.return (Error (err_msg e))
      | Ok text -> (
          let text = Jstr.to_string text in
          if status < 200 || status >= 300 then
            Fut.return (Error (error_of_body ~status text))
          else if String.trim text = "" then return Jv.null
          else
            match Json.decode (Jstr.v text) with
            | Ok j -> return j
            | Error e ->
                Fut.return (Error ("malformed JSON from GitHub: " ^ err_msg e)))
      )

let get ~token url = request ~token ~meth:"GET" url
let post ~token ~body url = request ~token ~meth:"POST" ~body url

(* {1 Domain types} *)

type file = {
  filename : string;
  previous_filename : string option;
  status : string;
      (** added, removed, modified, renamed, copied, changed, unchanged *)
  additions : int;
  deletions : int;
  patch : string option;  (** absent for binary files and very large diffs *)
  blob_url : string;
}

type pr = {
  pr_ref : pr_ref;
  title : string;
  author : string;
  state : string;
  draft : bool;
  merged : bool;
  html_url : string;
  base : string;
  head : string;
  additions : int;
  deletions : int;
  changed_files : int;
  files : file list;
  truncated : bool;  (** [true] when the PR has more files than we fetched *)
  approved_by : string list;
      (** logins whose most recent verdict on this PR is an approval *)
}

let file_of_json j =
  {
    filename = J.str j "filename";
    previous_filename = J.str_opt j "previous_filename";
    status = J.str ~default:"modified" j "status";
    additions = J.int j "additions";
    deletions = J.int j "deletions";
    patch = J.str_opt j "patch";
    blob_url = J.str j "blob_url";
  }

let max_pages = 10
let per_page = 100

let fetch_files ~token (r : pr_ref) : (file list * bool) fut =
  let rec page n acc =
    let url =
      Printf.sprintf "%s/repos/%s/%s/pulls/%d/files?per_page=%d&page=%d" api
        r.owner r.repo r.number per_page n
    in
    let* j = get ~token url in
    let batch = J.list file_of_json j in
    let acc = acc @ batch in
    if List.length batch < per_page then return (acc, false)
    else if n >= max_pages then return (acc, true)
    else page (n + 1) acc
  in
  page 1 []

(* Reviews, oldest first. GitHub keeps every review event, so a reviewer who
   approved and later asked for changes still has an APPROVED entry in here. *)
let fetch_reviews ~token (r : pr_ref) : (string * string) list fut =
  let rec page n acc =
    let url =
      Printf.sprintf "%s/repos/%s/%s/pulls/%d/reviews?per_page=%d&page=%d" api
        r.owner r.repo r.number per_page n
    in
    let* j = get ~token url in
    let batch =
      J.list (fun v -> (J.str (Jv.get v "user") "login", J.str v "state")) j
    in
    let acc = acc @ batch in
    if List.length batch < per_page || n >= max_pages then return acc
    else page (n + 1) acc
  in
  page 1 []

(* Who currently approves. Only a verdict supersedes an earlier one: a plain
   COMMENTED review leaves the reviewer's previous stance alone, so those are
   dropped before the last entry per reviewer is taken. *)
let approvers (reviews : (string * string) list) : string list =
  let decisive (_, state) =
    state = "APPROVED" || state = "CHANGES_REQUESTED" || state = "DISMISSED"
  in
  let latest =
    List.fold_left
      (fun acc (login, state) -> (login, state) :: List.remove_assoc login acc)
      []
      (List.filter decisive reviews)
  in
  List.sort compare
    (List.filter_map
       (fun (login, state) -> if state = "APPROVED" then Some login else None)
       latest)

let fetch_pr ~token (r : pr_ref) : pr fut =
  let url =
    Printf.sprintf "%s/repos/%s/%s/pulls/%d" api r.owner r.repo r.number
  in
  let* j = get ~token url in
  let* files, truncated = fetch_files ~token r in
  let* reviews = fetch_reviews ~token r in
  return
    {
      pr_ref = r;
      title = J.str j "title";
      author = J.str (Jv.get j "user") "login";
      state = J.str j "state";
      draft = J.bool j "draft";
      merged = J.bool j "merged";
      html_url =
        (match J.str_opt j "html_url" with Some u -> u | None -> ref_url r);
      base = J.str (Jv.get j "base") "ref";
      head = J.str (Jv.get j "head") "ref";
      additions = J.int j "additions";
      deletions = J.int j "deletions";
      changed_files = J.int j "changed_files";
      files;
      truncated;
      approved_by = approvers reviews;
    }

let approve ~token ?(body = "") (r : pr_ref) : unit fut =
  let url =
    Printf.sprintf "%s/repos/%s/%s/pulls/%d/reviews" api r.owner r.repo r.number
  in
  let payload =
    Jv.obj
      (Array.of_list
         (("event", Jv.of_string "APPROVE")
         :: (if body = "" then [] else [ ("body", Jv.of_string body) ])))
  in
  let* _ = post ~token ~body:(Jstr.to_string (Json.encode payload)) url in
  return ()

(* The token's own login, used to show who reviews are attributed to. *)
let whoami ~token : string fut =
  let* j = get ~token (api ^ "/user") in
  return (J.str j "login")

(* Whether an error produced by this module reports a credentials problem
   rather than something about the pull request.  The strings come from
   [error_of_body] above, so the shapes matched here are ours. *)
let is_auth_error msg =
  let contains needle =
    let n = String.length needle and m = String.length msg in
    let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
    go 0
  in
  (* 401 is a bad or expired token; 403 is a token without the needed scope. *)
  contains "HTTP 401" || contains "HTTP 403"
