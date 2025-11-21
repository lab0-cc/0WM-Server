(** This module implements functorized heaps *)

(** Module for heap indices *)
module type S = sig
  (** The type of heap indices *)
  type index

  (** Heap property *)
  val property : index -> index -> bool
end

(** Module for ordered types *)
module type O = sig
  (** The type of heap indices *)
  type t

  (** Total ordering over heap indices *)
  val compare : t -> t -> int
end

(** Module for max-heap indices *)
module Max (T : O) : S with type index = T.t

(** Module for min-heap indices *)
module Min (T : O) : S with type index = T.t

(** Functor building heap implementations *)
module Make (T : S) : sig
  (** The type of heap items *)
  type 'a item = { value : T.index; content : 'a }

  (** The type of heaps *)
  type 'a t = Empty | Heap of { item : 'a item; left : 'a t; right : 'a t; size : int }

  (** Add a value to a heap *)
  val add : T.index -> 'a -> 'a t -> 'a t

  (** Pop the top value of a heap and return the new heap *)
  val pop : 'a t -> 'a item * 'a t

  (** Drop the top value of a heap and return the new heap *)
  val drop : 'a t -> 'a t

  (** Return the top value of a heap *)
  val bound : 'a t -> T.index

  (** Return the heap size *)
  val size : 'a t -> int

  (** Sort a heap *)
  val sort : 'a t -> 'a item list
end
