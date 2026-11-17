(* Reading the fields of decoded JSON without a codec library. A missing field
   or a null reads as the default rather than failing. *)

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

(* A string reached through nested objects, tolerating a missing step: the
   repository behind a pull request's base is absent when it has been
   deleted. *)
let str_path j path =
  let rec go j = function
    | _ when Jv.is_none j -> ""
    | [] -> Jstr.to_string (Jv.to_jstr j)
    | n :: tl -> go (Jv.get j n) tl
  in
  go j path
