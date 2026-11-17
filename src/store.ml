open Brr

let store = Brr_io.Storage.local G.window

let get key =
  Option.map Jstr.to_string (Brr_io.Storage.get_item store (Jstr.v key))

let set key v = ignore (Brr_io.Storage.set_item store (Jstr.v key) (Jstr.v v))
