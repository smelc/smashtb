(* The browser's notifications. Pages on http://localhost count as secure, so
   they may use them; the user still has to allow it once, from a click. *)

open Brr
open Brr_io

let supported () = not (Jv.is_none (Jv.get Jv.global "Notification"))

type permission = Unsupported | Ask | Granted | Denied

let permission () =
  if not (supported ()) then Unsupported
  else
    let p = Notification.permission () in
    if Jstr.equal p Notification.Permission.granted then Granted
    else if Jstr.equal p Notification.Permission.denied then Denied
    else Ask

(* Must run from a click: browsers ignore the request otherwise. *)
let request k =
  if supported () then
    Fut.await (Notification.request_permission ()) (fun _ -> k (permission ()))
  else k Unsupported

(* Show a notification, if allowed. One with the same [tag] as a notification
   still on screen replaces it, still alerting, rather than piling up. *)
let show ~title ~body ~tag ~on_click =
  if permission () = Granted then begin
    (* Created by hand: Brr's Notification.opts drops the tag, and browsers
       refuse renotify without one. *)
    let opts =
      Jv.obj
        [|
          ("body", Jv.of_string body);
          ("tag", Jv.of_string tag);
          ("renotify", Jv.true');
        |]
    in
    let n =
      Notification.of_jv
        (Jv.new'
           (Jv.get Jv.global "Notification")
           [| Jv.of_string title; opts |])
    in
    Jv.set (Notification.to_jv n) "onclick"
      (Jv.callback ~arity:1 (fun _ ->
           ignore (Jv.call (Window.to_jv G.window) "focus" [||]);
           on_click ();
           Notification.close n))
  end
