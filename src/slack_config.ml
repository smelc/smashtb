(* What to listen to on Slack: the channels to watch, by workspace, and the
   pattern a message must match. Kept in local storage, so a restart needs no
   reconfiguring. *)

open Brr
module Smap = Map.Make (String)

type channel = {
  id : string;
  name : string;
      (** remembered so a channel can still be shown once Slack stops listing it
      *)
}

type t = {
  channels : channel list Smap.t;  (** by workspace id *)
  pattern : string;  (** a JavaScript regular expression; "" matches nothing *)
  ignore_case : bool;
}

let empty = { channels = Smap.empty; pattern = ""; ignore_case = true }
let pattern c = c.pattern
let ignore_case c = c.ignore_case
let with_pattern c pattern = { c with pattern }
let with_ignore_case c ignore_case = { c with ignore_case }
let key = "smashtb.slack"

(* {1 Storage} *)

let to_jv c =
  let chan ch =
    Jv.obj [| ("id", Jv.of_string ch.id); ("name", Jv.of_string ch.name) |]
  in
  let workspace (team, chans) =
    Jv.obj [| ("id", Jv.of_string team); ("channels", Jv.of_list chan chans) |]
  in
  Jv.obj
    [|
      ("workspaces", Jv.of_list workspace (Smap.bindings c.channels));
      ("pattern", Jv.of_string c.pattern);
      ("ignore_case", Jv.of_bool c.ignore_case);
    |]

let of_jv j =
  let module J = Jread in
  let chan j = { id = J.str j "id"; name = J.str j "name" } in
  let channels =
    List.fold_left
      (fun m w ->
        let chans = J.list chan (Jv.get w "channels") in
        if chans = [] then m else Smap.add (J.str w "id") chans m)
      Smap.empty
      (J.list Fun.id (Jv.get j "workspaces"))
  in
  let ignore_case =
    let v = Jv.get j "ignore_case" in
    Jv.is_none v || Jv.to_bool v
  in
  { channels; pattern = J.str j "pattern"; ignore_case }

let load () =
  match Option.map (fun s -> Json.decode (Jstr.v s)) (Store.get key) with
  | Some (Ok j) when not (Jv.is_none j) -> of_jv j
  | None | Some _ -> empty

let save c = Store.set key (Jstr.to_string (Json.encode (to_jv c)))

(* {1 Channels} *)

let watched c ~team = Option.value (Smap.find_opt team c.channels) ~default:[]

(* Every watched channel, whatever its workspace. *)
let watched_anywhere c = List.concat_map snd (Smap.bindings c.channels)
let is_watched c ~team id = List.exists (fun ch -> ch.id = id) (watched c ~team)

let set_watched c ~team ch on =
  let others = List.filter (fun x -> x.id <> ch.id) (watched c ~team) in
  let chans = if on then ch :: others else others in
  let chans = List.sort (fun a b -> compare a.name b.name) chans in
  {
    c with
    channels =
      (if chans = [] then Smap.remove team c.channels
       else Smap.add team chans c.channels);
  }

(* {1 Pattern} *)

type regex = Jv.t

let regexp = Jv.get Jv.global "RegExp"

(* The compiled pattern: [Ok None] when there is none yet, [Error] with the
   browser's own complaint when it is not a valid regular expression. *)
let compile c =
  if String.trim c.pattern = "" then Ok None
  else
    try
      Ok
        (Some
           (Jv.new' regexp
              [|
                Jv.of_string c.pattern;
                Jv.of_string (if c.ignore_case then "i" else "");
              |]))
    with Jv.Error e -> Error (Jstr.to_string (Jv.Error.message e))

(* [re] has no "g" flag, so [test] keeps no state between calls. *)
let test re text = Jv.to_bool (Jv.call re "test" [| Jv.of_string text |])

let matches c text =
  match compile c with Ok (Some re) -> test re text | _ -> false

(* [text] cut into pieces, each saying whether [re] matched it. The cutting is
   done on the JavaScript string: match positions count UTF-16 units, which
   are not OCaml's bytes. *)
let segments re text =
  let flags = Jstr.to_string (Jv.to_jstr (Jv.get re "flags")) in
  let g = Jv.new' regexp [| re; Jv.of_string (flags ^ "g") |] in
  let s = Jstr.v text in
  let piece a b hit acc =
    if b <= a then acc
    else (Jstr.to_string (Jstr.slice ~start:a ~stop:b s), hit) :: acc
  in
  let rec go from acc =
    let m = Jv.call g "exec" [| Jv.of_jstr s |] in
    if Jv.is_null m then List.rev (piece from (Jstr.length s) false acc)
    else
      let start = Jv.to_int (Jv.get m "index") in
      let stop = start + Jstr.length (Jv.to_jstr (Jv.Jarray.get m 0)) in
      if stop = start then begin
        (* An empty match: step over it, or exec would find it forever. *)
        Jv.set g "lastIndex" (Jv.of_int (stop + 1));
        go from acc
      end
      else go stop (piece start stop true (piece from start false acc))
  in
  go 0 []
