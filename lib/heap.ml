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
  type 'a item = { value : T.index; content : 'a }
  type 'a t = Empty | Heap of { item : 'a item; left : 'a t; right : 'a t; size : int }

  let rec add value content = function
    | Empty -> Heap { item = { value; content }; left = Empty; right = Empty; size = 1 }
    | Heap { item; left; right; size } when T.property value item.value ->
        Heap { item = { value; content }; left = add item.value item.content right; right = left;
               size = size + 1 }
    | Heap { item; left; right; size } ->
        Heap { item; left = add value content right; right = left; size = size + 1 }

  let rec pop = function
    | Empty -> invalid_arg "pop"
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
    | Empty -> invalid_arg "bound"
    | Heap { item; _ } -> item.value

  let size = function
    | Empty -> 0
    | Heap { size; _ } -> size

  let rec merge h h' = match h, h' with
    | Empty, h | h, Empty -> h
    | Heap h, Heap { item = { value; _ }; _ } when T.property h.item.value value ->
        Heap { h with left = merge h.right h'; right = h.left }
    | _, Heap h' -> Heap { h' with left = merge h h'.right; right = h'.left }

  let sort h =
    let rec sort_rec acc = function
      | Empty -> acc
      | Heap { item; left; right; _ } -> merge left right |> sort_rec (item::acc) in
    sort_rec [] h
end
