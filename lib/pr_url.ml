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

(* Split free-form text on whitespace and commas, then parse each token.
   Returns the references found and the tokens that made no sense. *)
let parse_many (text : string) : t list * string list =
  let is_sep c = c = '\n' || c = '\r' || c = ' ' || c = '\t' || c = ',' in
  let tokens = String.split_on_char '\n' (String.map (fun c -> if is_sep c then '\n' else c) text) in
  let ok, bad =
    List.fold_left
      (fun (ok, bad) tok ->
        let tok = String.trim tok in
        if tok = "" then (ok, bad)
        else match parse tok with Some r -> (r :: ok, bad) | None -> (ok, tok :: bad))
      ([], []) tokens
  in
  (List.rev ok, List.rev bad)
