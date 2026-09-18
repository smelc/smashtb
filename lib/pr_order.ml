(* Ordering the pull requests in the list: the ones closest to landing first.

   A pull request that targets the default branch lands next, so it comes
   first. One that targets the branch of another loaded pull request lands
   only after it, so it comes one step further down, and so on up the stack:
   a stack of pull requests reads top to bottom in the order it will merge.
   Pull requests at the same depth are ordered by number, smallest first.

   Depth is read off the pull requests currently loaded, because that is all
   there is to go on: a pull request whose base is neither the default branch
   nor the branch of a loaded pull request has no known place in a stack, and
   goes last. A pull request still being fetched looks the same way and takes
   its place once GitHub answers. *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

type t = {
  owner : string;
  repo : string;
  number : int;
  base : string;  (** the branch it merges into *)
  head : string;  (** the branch it carries *)
  base_is_default : bool;
      (** whether [base] is the repository's default branch *)
}

(* What is known about a pull request before GitHub has answered: enough to
   break ties, not enough to place it in a stack. *)
let pending (r : Pr_url.t) =
  {
    owner = r.owner;
    repo = r.repo;
    number = r.number;
    base = "";
    head = "";
    base_is_default = false;
  }

let id t = Printf.sprintf "%s/%s#%d" t.owner t.repo t.number

(* Branch names only mean anything within one repository, so the repository is
   part of the key a base is looked up under. *)
let branch t name = Printf.sprintf "%s/%s:%s" t.owner t.repo name

(* The depth given to a pull request whose stack cannot be traced. *)
let unknown = max_int

(* The loaded pull requests, indexed by the branch each one carries. *)
let by_head (prs : t list) : t SMap.t =
  (* Smallest number first, so that two pull requests off the same branch
     resolve to the same one however they arrived. *)
  List.fold_left
    (fun m t ->
      let k = branch t t.head in
      if t.head = "" || SMap.mem k m then m else SMap.add k t m)
    SMap.empty
    (List.sort (fun a b -> compare a.number b.number) prs)

(* The loaded pull request whose branch [t] targets, when [t] is stacked on
   one rather than aimed at the default branch. *)
let parent (prs : t list) (t : t) : t option =
  if t.base_is_default || t.base = "" then None
  else SMap.find_opt (branch t t.base) (by_head prs)

let depths (prs : t list) : int SMap.t =
  let by_head = by_head prs in
  let rec depth seen t =
    if t.base_is_default then 0
    else if SSet.mem (id t) seen then unknown (* a cycle: nothing lands *)
    else
      match SMap.find_opt (branch t t.base) by_head with
      | None -> unknown
      | Some parent ->
          let d = depth (SSet.add (id t) seen) parent in
          if d = unknown then unknown else d + 1
  in
  List.fold_left
    (fun m t -> SMap.add (id t) (depth SSet.empty t) m)
    SMap.empty prs

(* [sort key items] puts [items] in merge order, reading each one's pull
   request with [key]. *)
let sort (key : 'a -> t) (items : 'a list) : 'a list =
  let ds = depths (List.map key items) in
  let rank t =
    match SMap.find_opt (id t) ds with None -> unknown | Some d -> d
  in
  let cmp a b =
    let a = key a and b = key b in
    compare
      (rank a, a.number, a.owner, a.repo)
      (rank b, b.number, b.owner, b.repo)
  in
  List.stable_sort cmp items
