(* Asynchronous computations that can fail with a message for people, such as
   calls to GitHub or to Slack. *)

type 'a t = ('a, string) result Fut.t

let return v = Fut.return (Ok v)
let fail msg = Fut.return (Error msg)

let ( let* ) x f =
  Fut.bind x (function Error e -> Fut.return (Error e) | Ok v -> f v)

(* The message of a JavaScript error, such as a failed fetch. *)
let error_message e = Jstr.to_string (Jv.Error.message e)
