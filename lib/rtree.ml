type t = Node of Geo.box * t list | Leaf of string [@@marshal]

open Lwt.Syntax
open Lwt.Infix

module Queue = Heap.Make (Heap.Min (Float))
module Max_heap = Heap.Make (Heap.Max (Float))

let singleton id = Leaf id

let max_children = 4

let geo_of store = function
  | Node (box, _) -> Geo.Bounding_box box |> Lwt.return
  | Leaf ref ->
      let* o = Store.find_object ref store in
      Lwt.return o.structure

let sort ?(limit=100) p tree store =
  let rec sort_rec acc =
    let bound = match acc with
      | Max_heap.Empty -> Float.infinity
      | Max_heap.Heap _ -> Max_heap.bound acc in
    function
    | Queue.Empty -> Lwt.return acc
    | queue -> match Queue.pop queue with
        | { value; _ }, _ when value > bound && Max_heap.size acc = limit ->
            (* If the distance is already worse than our current worst best, stop *)
            Lwt.return acc
        | { value; content = Leaf id }, q ->
            (* If we are dealing with a leaf node, add it and continue processing the queue *)
            let acc = Max_heap.add value id acc in
            let acc = if Max_heap.size acc > limit then Max_heap.drop acc else acc in
            sort_rec acc q
        | { content = Node (_, children); _ }, q ->
            (* If we are dealing with an internal node, add its children to the queue *)
            Lwt_list.fold_left_s
              (fun q node ->
                 let* geo = geo_of store node in
                 Queue.add (Geo.obj_distance p geo) node q |> Lwt.return)
              q children
            >>= sort_rec acc in
  let* geo = geo_of store tree in
  let q = Queue.(add (Geo.obj_distance p geo) tree Empty) in
  Max_heap.(sort_rec Empty q >|= sort >>= Lwt_list.map_p (fun { value; content } -> Lwt.return (value, content)))

let lazy_l x = lazy (Lwt.return x)

let add id tree store =
  let* shape = Leaf id |> geo_of store in
  let rec add_rec id bb = function
    | Leaf id' ->
        (* If this is a leaf, create a branch node *)
        Node (bb, [Leaf id; Leaf id']) |> Lwt.return
    | Node (_, children) when List.length children < max_children ->
        (* If the shape can fit in the tree, we can use an O(n) heuristic *)
        let rec balance visited candidate area = function
          | node::tl ->
              let* obj = geo_of store node in
              let bb = Geo.merge_bounding_boxes [obj; shape] in
              let area' = Geo.box_area bb in
              if area' < area
              then balance (lazy_l node::visited) (lazy (add_rec id bb node)::visited) area' tl
              else balance (lazy_l node::visited) (lazy_l node::candidate) area tl
          | [] ->
              let* l = Lwt_list.rev_map_p Lazy.force candidate in
              Node (bb, l) |> Lwt.return in
        balance [] [lazy_l (Leaf id)] Geo.(bounding_box shape |> box_area) children
    | Node (_, children) ->
        (* Else, we use an O(n²) heuristic *)
        let rec worst_pair = function
          | node::node'::[] ->
              let* bb = geo_of store node >|= Geo.bounding_box in
              let* bb' = geo_of store node' >|= Geo.bounding_box in
              Lwt.return (((node, bb), (node', bb')), Geo.(merge_pure_bounding_boxes bb bb' |> box_area))
          | node::tl ->
              let* bb = geo_of store node >|= Geo.bounding_box in
              let* l = worst_pair tl in
              Lwt_list.fold_left_s
                (fun (_, area as acc) node' ->
                  let* bb' = geo_of store node' >|= Geo.bounding_box in
                  let area' = Geo.(merge_pure_bounding_boxes bb bb' |> box_area) in
                  if area' > area
                  then Lwt.return (((node, bb), (node', bb')), area')
                  else Lwt.return acc) l tl
          | _ -> failwith "Unreachable" in
        (* First, we get the worst pair of nodes, which we then use to split the tree *)
        let children = Leaf id::children in
        let* (((node', bb'), (node'', bb'')), _) = worst_pair children in
        let others = List.filter (fun node -> node != node' && node != node'') children in
        (* We wrap these nodes into their own subtrees *)
        let rec insert_others (bb', node', bb'', node'') = function
          | [] -> Node (bb, [Node (bb', node'); Node (bb'', node'')]) |> Lwt.return
          | l ->
              let rec insert_farthest (node, (bb:Geo.box), area, left as acc) = function
                | hd::tl ->
                    let* shape = geo_of store hd in
                    let bb' = Geo.(merge_bounding_boxes [shape; Bounding_box bb']) in
                    let bb'' = Geo.(merge_bounding_boxes [shape; Bounding_box bb'']) in
                    let area' = Geo.box_area bb' in
                    let area'' = Geo.box_area bb'' in
                    if area' < area && area'' < area
                    then insert_farthest acc tl
                    else if area' < area''
                         then insert_farthest (hd, bb', area'', true) tl
                         else insert_farthest (hd, bb'', area', false) tl
                | [] ->
                    let acc =
                      if left
                      then (bb, node::node', bb'', node'')
                      else (bb', node', bb, node::node'') in
                    insert_others acc (List.filter (fun n -> n != node) l) in
              insert_farthest (tree, bb, 0., false) l in
        (* And we insert the remaining nodes into these two subtrees *)
        insert_others (bb', [node'], bb'', [node'']) others in
  let* shape' = geo_of store tree in
  add_rec id Geo.(merge_bounding_boxes [shape; shape']) tree

let remove id tree store =
  (* Returns Some _ if something changed, else None *)
  let rec remove_rec' changed acc = function
    | l when changed ->
        let children = List.rev_append acc l in
        let* objects = Lwt_list.map_p (geo_of store) children in
        let bb = Geo.merge_bounding_boxes objects in
        Some (Node (bb, children)) |> Lwt.return
    | hd::tl -> begin match%lwt remove_rec hd with
        | changed, None -> remove_rec' changed acc tl
        | changed, Some n -> remove_rec' changed (n::acc) tl
      end
    | [] when changed ->
        let* objects = Lwt_list.map_p (geo_of store) acc in
        let bb = Geo.merge_bounding_boxes objects in
        Some (Node (bb, acc)) |> Lwt.return
    | [] -> Lwt.return None

  and remove_rec = function
    | Leaf id' when id' = id -> Lwt.return (true, None)
    | Leaf _ as n -> Lwt.return (false, Some n)
    | Node (_, Leaf id'::n::[]) when id' = id -> Lwt.return (true, Some n)
    | Node (_, n::Leaf id'::[]) when id' = id -> Lwt.return (true, Some n)
    | Node (_, l) as n -> match%lwt remove_rec' false [] l with
        | None -> Lwt.return (false, Some n)
        | n -> Lwt.return (true, n) in

  let* (_, t) = remove_rec tree in Lwt.return t

let to_list =
  let rec to_list_rec acc = function
    | Node (_, children) -> List.fold_left to_list_rec acc children
    | Leaf ref -> ref::acc in
  to_list_rec []
