(* Small DOM helpers shared by the pages. *)

open Brr

let cls c = At.class' (Jstr.v c)
let txt s = El.txt' s
let attr n v = At.v (Jstr.v n) (Jstr.v v)

let on_click el f =
  ignore (Ev.listen Ev.click (fun ev -> f ev) (El.as_target el))

let set_disabled el b =
  El.set_at (Jstr.v "disabled") (if b then Some Jstr.empty else None) el

let set_open el b =
  El.set_at (Jstr.v "open") (if b then Some Jstr.empty else None) el

let details ?(at = []) children = El.v ~at (Jstr.v "details") children
let summary ?(at = []) children = El.v ~at (Jstr.v "summary") children

let button ?(classes = "btn") ?title label f =
  let at = [ cls classes; attr "type" "button" ] in
  let at =
    match title with None -> at | Some t -> At.title (Jstr.v t) :: at
  in
  let b = El.button ~at [ txt label ] in
  on_click b (fun ev ->
      Ev.stop_propagation ev;
      Ev.prevent_default ev;
      f ());
  b

let plural n one many = if n = 1 then one else many
