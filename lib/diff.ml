(* Parsing of the unified-diff patches returned by the GitHub API.

   GitHub gives us one patch per file, made of hunks such as

     @@ -12,7 +12,9 @@ let f x =
      context line
     -removed line
     +added line

   We turn that into a list of lines carrying their old and new line
   numbers, which is all the diff view needs. *)

type kind =
  | Context
  | Added
  | Removed
  | Hunk  (** the [@@ ... @@] header itself *)
  | Meta  (** anything else, e.g. "\ No newline at end of file" *)

type line = {
  kind : kind;
  old_no : int option;
  new_no : int option;
  text : string;
}

type stats = { added : int; removed : int }

(* "@@ -old[,count] +new[,count] @@ section" -> (old, new) *)
let parse_hunk_header s =
  let n = String.length s in
  let is_digit c = c >= '0' && c <= '9' in
  let rec find c i = if i >= n then None else if s.[i] = c then Some i else find c (i + 1) in
  let read_int i =
    let stop = ref i in
    while !stop < n && is_digit s.[!stop] do incr stop done;
    if !stop = i then None else Some (int_of_string (String.sub s i (!stop - i)), !stop)
  in
  match find '-' 0 with
  | None -> None
  | Some minus -> (
      match read_int (minus + 1) with
      | None -> None
      | Some (old_start, after_old) -> (
          match find '+' after_old with
          | None -> None
          | Some plus -> (
              match read_int (plus + 1) with
              | None -> None
              | Some (new_start, _) -> Some (old_start, new_start))))

let drop_first s = if String.length s = 0 then s else String.sub s 1 (String.length s - 1)

let split_lines s =
  match String.split_on_char '\n' s with
  (* A patch does not end with a newline, but be tolerant if it does. *)
  | [] -> []
  | lines -> (
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines)

let parse (patch : string) : line list =
  let rec go lines old_no new_no acc =
    match lines with
    | [] -> List.rev acc
    | l :: rest ->
        let keep kind old_no' new_no' text = { kind; old_no = old_no'; new_no = new_no'; text } in
        if String.length l >= 2 && l.[0] = '@' && l.[1] = '@' then
          let old_no, new_no =
            match parse_hunk_header l with Some (o, n) -> (o, n) | None -> (old_no, new_no)
          in
          go rest old_no new_no (keep Hunk None None l :: acc)
        else if l = "" then
          (* An empty context line: the leading space was stripped somewhere. *)
          go rest (old_no + 1) (new_no + 1) (keep Context (Some old_no) (Some new_no) "" :: acc)
        else
          match l.[0] with
          | '+' -> go rest old_no (new_no + 1) (keep Added None (Some new_no) (drop_first l) :: acc)
          | '-' -> go rest (old_no + 1) new_no (keep Removed (Some old_no) None (drop_first l) :: acc)
          | ' ' ->
              go rest (old_no + 1) (new_no + 1)
                (keep Context (Some old_no) (Some new_no) (drop_first l) :: acc)
          | _ -> go rest old_no new_no (keep Meta None None l :: acc)
  in
  go (split_lines patch) 1 1 []
