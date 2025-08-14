module type S = sig
  type index
  val property : index -> index -> bool
end

module type O = sig
  type t
  val compare : t -> t -> int
end

module Max (T : O) = struct
  type index = T.t
  let property a b = T.compare a b > 0
end

module Min (T : O) = struct
  type index = T.t
  let property a b = T.compare a b < 0
end

module Make (T : S) = struct
  type 'a item = { value: T.index; content: 'a }
  type 'a t = Empty | Heap of { item: 'a item; left: 'a t; right: 'a t; size: int }

  let rec add value content = function
    | Empty -> Heap { item = { value; content }; left = Empty; right = Empty; size = 1 }
    | Heap { item; left; right; size } when T.property value item.value ->
        Heap { item = { value; content }; left = add item.value item.content right; right = left;
               size = size + 1 }
    | Heap { item; left; right; size } ->
        Heap { item; left = add value content right; right = left; size = size + 1 }

  let rec pop = function
    | Empty -> failwith "pop"
    | Heap { item; left = Empty; right; _ } -> (item, right)
    | Heap { item; left; right = Empty; _ } -> (item, left)
    | Heap { item; left = Heap { item = l; _ } as left; right = Heap { item = r; _ } as right;
             size }
      when T.property l.value r.value ->
        (item, Heap { item = l; left = drop left; right; size = size - 1 })
    | Heap { item; left; right = Heap { item = r; _ } as right; size } ->
        (item, Heap { item = r; left; right = drop right; size = size - 1 })

  and drop h = let (_, h) = pop h in h

  let bound = function
    | Empty -> failwith "bound"
    | Heap { item; _ } -> item.value

  let size = function
    | Empty -> 0
    | Heap { size; _ } -> size

  let rec rev_merge acc l l' = match l, l' with
    | hd::_, hd'::tl' when T.property hd.value hd'.value -> rev_merge (hd'::acc) l tl'
    | hd::tl, _ -> rev_merge (hd::acc) tl l'
    | _, _ -> l @ l' @ acc
  let rec rev_sort = function
    | Empty -> []
    | Heap { item; left; right; _ } -> item::rev_merge [] (rev_sort left) (rev_sort right)

  let sort h = rev_sort h |> List.rev
end
