(* Recognising the pull request references people paste. *)

type t = { owner : string; repo : string; number : int }

let to_string r = Printf.sprintf "%s/%s#%d" r.owner r.repo r.number
let to_url r = Printf.sprintf "https://github.com/%s/%s/pull/%d" r.owner r.repo r.number

let non_empty = List.filter (fun s -> s <> "")

let cut c s =
  match String.index_opt s c with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let drop_scheme s =
  match String.index_opt s ':' with
  | Some i when i + 2 < String.length s && s.[i + 1] = '/' && s.[i + 2] = '/' ->
      String.sub s (i + 3) (String.length s - i - 3)
  | _ -> s

let parse_url s =
  let body, _ = cut '#' s in
  let body, _ = cut '?' body in
  let segs = non_empty (String.split_on_char '/' (drop_scheme body)) in
  let rec find = function
    | owner :: repo :: kind :: number :: _ when kind = "pull" || kind = "pulls" -> (
        match int_of_string_opt number with
        | Some number when number > 0 -> Some { owner; repo; number }
        | _ -> None)
    | _ :: tl -> find tl
    | [] -> None
  in
  find segs

(* Accepts the shapes people actually paste:
     https://github.com/owner/repo/pull/12       (with any /files or #note suffix)
     github.com/owner/repo/pull/12
     https://api.github.com/repos/owner/repo/pulls/12
     owner/repo#12 *)
let parse (s : string) : t option =
  let s = String.trim s in
  if s = "" then None
  else
    match cut '#' s with
    | short, Some after when (not (String.contains short ':')) && String.contains short '/' -> (
        (* owner/repo#12.  A real URL fragment always sits behind a scheme or a
           longer path, both of which fail the two-segment match below and fall
           through to [parse_url]. *)
        match
          (non_empty (String.split_on_char '/' short), int_of_string_opt (String.trim after))
        with
        | [ owner; repo ], Some number when number > 0 -> Some { owner; repo; number }
        | _ -> parse_url s)
    | _ -> parse_url s

(* Characters that can appear inside a pull request reference. Everything else
   ends the token, which is what makes pasted prose work: Slack wraps links in
   <angle brackets> and appends |labels, markdown wraps them in [](), and
   sentences put commas and full stops against them. *)
let is_ref_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '/' | ':' | '.' | '-' | '_' | '#' | '~' | '?' | '=' | '&' | '%' | '+' -> true
  | _ -> false

let tokens text =
  let n = String.length text in
  let emit start stop acc =
    if stop > start then String.sub text start (stop - start) :: acc else acc
  in
  let rec go i start acc =
    if i >= n then List.rev (emit start n acc)
    else if is_ref_char text.[i] then go (i + 1) start acc
    else go (i + 1) (i + 1) (emit start i acc)
  in
  go 0 0 []

(* Punctuation that is a reference character in the middle of a link but is
   only ever sentence noise at the end of one. *)
let rec trim_tail s =
  let n = String.length s in
  if n = 0 then s
  else
    match s.[n - 1] with
    | '.' | ',' | ':' | ';' | '#' | '-' | '_' | '?' | '=' | '&' | '%' | '+' | '~' | '/' ->
        trim_tail (String.sub s 0 (n - 1))
    | _ -> s

(* Pull every distinct pull request reference out of arbitrary text, in the
   order they appear. Anything that is not one is ignored, so a whole Slack
   message can be pasted in and only the links matter. *)
let extract (text : string) : t list =
  let keep (seen, found) tok =
    match parse (trim_tail tok) with
    | None -> (seen, found)
    | Some r ->
        let key = to_string r in
        if List.mem key seen then (seen, found) else (key :: seen, r :: found)
  in
  let _, found = List.fold_left keep ([], []) (tokens text) in
  List.rev found
