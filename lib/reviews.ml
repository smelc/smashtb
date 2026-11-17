let decisive state =
  state = "APPROVED" || state = "CHANGES_REQUESTED" || state = "DISMISSED"

let approvers reviews =
  (* The last decisive verdict per reviewer, built back to front: a later
     entry replaces an earlier one. *)
  let latest =
    List.fold_left
      (fun acc (login, state) -> (login, state) :: List.remove_assoc login acc)
      []
      (List.filter (fun (_, state) -> decisive state) reviews)
  in
  List.sort_uniq compare
    (List.filter_map
       (fun (login, state) -> if state = "APPROVED" then Some login else None)
       latest)
