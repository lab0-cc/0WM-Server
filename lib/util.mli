(** This module implements helper functions *)

(** Epsilon value *)
val eps : float

(** ln / log conversion constant *)
val c10 : float

(** Run List.map in parallel *)
val par_map : domains:int -> zero:'a -> ('b -> 'a) -> 'b array -> 'a array

(** Run List.init in parallel *)
val par_init : domains:int -> zero:'a -> int -> (int -> 'a) -> 'a array

(** Concatenate strings *)
val cat : string list -> string

(** Fold over a range of integers *)
val fold_range : ('a -> int -> 'a) -> 'a -> int -> int -> 'a

(** Quickselect algorithm *)
val quickselect : int -> 'a list -> 'a
