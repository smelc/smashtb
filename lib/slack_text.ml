(* Slack's message markup, read for people: what a notification shows.

   In the text Slack sends, everything between < and > is markup: links
   (<https://x|label>), mentions (<@U123>), channels (<#C123|name>) and
   special mentions (<!here>). A literal <, > or & is sent as &lt;, &gt; or
   &amp;. See https://docs.slack.dev/messaging/formatting-message-text *)

(* "a|b" -> ("a", Some "b") *)
let split_label s =
  match String.index_opt s '|' with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else
      let rest = String.sub s i (n - i) in
      let starts p =
        String.length rest >= String.length p
        && String.sub rest 0 (String.length p) = p
      in
      if starts "&lt;" then (
        Buffer.add_char b '<';
        go (i + 4))
      else if starts "&gt;" then (
        Buffer.add_char b '>';
        go (i + 4))
      else if starts "&amp;" then (
        Buffer.add_char b '&';
        go (i + 5))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let drop_prefix ~prefix s =
  let n = String.length prefix in
  if String.length s >= n && String.sub s 0 n = prefix then
    Some (String.sub s n (String.length s - n))
  else None

(* What one piece of markup, without its brackets, reads as. [user] names a
   user id when it can; [addresses] keeps a labelled link's address. *)
let render_markup ~user ~addresses m =
  let target, label = split_label m in
  let rest s = String.sub s 1 (String.length s - 1) in
  if target = "" then Option.value label ~default:""
  else
    match target.[0] with
    | '@' -> (
        match label with
        | Some l -> "@" ^ l
        | None ->
            let id = rest target in
            "@" ^ Option.value (user id) ~default:id)
    | '#' -> "#" ^ Option.value label ~default:(rest target)
    | '!' -> (
        match label with
        | Some l -> l
        | None -> (
            (* <!here>, <!channel>, <!everyone>, or <!subteam^ID> without a
               name. *)
            match String.index_opt target '^' with
            | None -> "@" ^ rest target
            | Some i -> "@" ^ String.sub target 1 (i - 1)))
    | _ -> (
        let mail = drop_prefix ~prefix:"mailto:" target in
        match (label, mail) with
        | None, Some address -> address
        | None, None -> target
        | Some l, Some _ -> l
        | Some l, None when not addresses -> l
        | Some l, None ->
            (* Slack labels a pasted link with the address minus its scheme:
               no need to say it twice. *)
            let bare =
              List.find_map
                (fun prefix -> drop_prefix ~prefix target)
                [ "https://"; "http://" ]
            in
            if l = target || Some l = bare then target
            else Printf.sprintf "%s (%s)" l target)

(* [text] as plain text. *)
let plain ?(user = fun _ -> None) ?(addresses = false) text =
  let b = Buffer.create (String.length text) in
  let n = String.length text in
  let rec go i =
    if i >= n then ()
    else if text.[i] = '<' then (
      match String.index_from_opt text i '>' with
      | None -> Buffer.add_string b (String.sub text i (n - i))
      | Some j ->
          Buffer.add_string b
            (render_markup ~user ~addresses
               (String.sub text (i + 1) (j - i - 1)));
          go (j + 1))
    else begin
      Buffer.add_char b text.[i];
      go (i + 1)
    end
  in
  go 0;
  unescape (Buffer.contents b)

(* The user ids [text] mentions without giving their names, in order of first
   appearance. *)
let user_mentions text =
  let n = String.length text in
  let rec go i acc =
    match String.index_from_opt text i '<' with
    | None -> List.rev acc
    | Some i when i + 1 < n && text.[i + 1] = '@' -> (
        match String.index_from_opt text i '>' with
        | None -> List.rev acc
        | Some j ->
            let id, label = split_label (String.sub text (i + 2) (j - i - 2)) in
            let acc =
              if label = None && id <> "" && not (List.mem id acc) then
                id :: acc
              else acc
            in
            go (j + 1) acc)
    | Some i -> go (i + 1) acc
  in
  if n = 0 then [] else go 0 []

(* At most [max] bytes of [s], cut on a character boundary, with an ellipsis
   when something was left out. *)
let excerpt max s =
  if String.length s <= max then s
  else
    (* Back up to the start of a UTF-8 character: continuation bytes are
       10xxxxxx. *)
    let rec cut i =
      if i > 0 && Char.code s.[i] land 0xC0 = 0x80 then cut (i - 1) else i
    in
    String.sub s 0 (cut max) ^ "\xe2\x80\xa6"

(* The address of a message in Slack, from the workspace's address as
   auth.test gives it (https://name.slack.com/). A reply opens in its
   thread. *)
let permalink ~workspace ~channel ~ts ~thread_ts =
  let workspace =
    if String.ends_with ~suffix:"/" workspace then
      String.sub workspace 0 (String.length workspace - 1)
    else workspace
  in
  let p = "p" ^ String.concat "" (String.split_on_char '.' ts) in
  let base = Printf.sprintf "%s/archives/%s/%s" workspace channel p in
  match thread_ts with
  | Some t when t <> ts ->
      Printf.sprintf "%s?thread_ts=%s&cid=%s" base t channel
  | _ -> base
