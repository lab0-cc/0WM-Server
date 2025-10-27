(** This module implements linear algebra primitives *)

(** The type of 2D points *)
type point = { p_x: float; p_y: float } [@@marshal]

(** The type of 3D points *)
type point3 = { p3_x: float; p3_y: float; p3_z: float } [@@marshal]

module Vector2 : sig
  (** Get the vector from the first point to the second *)
  val of_points : point -> point -> point

  (** 2D cross product (or 2×2 matrix determinant) *)
  val cross : point -> point -> float

  (** Scale a vector *)
  val scaled : float -> point -> point

  (** Add a vector to a point *)
  val plus : point -> point -> point

  (** Negate a vector *)
  val neg : point -> point
end

module Point2 : sig
  (** Get the square distance between two points *)
  val sqdist : point -> point -> float

  (** Get the distance between two points *)
  val dist : point -> point -> float

  (** Interpolate between two points *)
  val lerp : point -> point -> float -> point
end

module Segment2 : sig
  (** The type of segments *)
  type t = { s_start : point; s_end : point } [@@marshal]

  (** Get the segment from the first point to the second *)
  val of_points : point -> point -> t

  (** The type of segment intersections *)
  type intersection = Parallel | Outside | Inside of point

  (** Intersect two segments *)
  val intersect : t -> t -> intersection

  (** Check if two segments cross *)
  val crosses : t -> t -> bool

  (** Mirror a point *)
  val mirror_p : point -> t -> point

  (** Get the closest point on a segment *)
  val closest : point -> t -> point

  (** Get the square distance between a point and a segment *)
  val sqdist_p : point -> t -> float
end

module Box2 : sig
  (** The type of boxes *)
  type t = { b_min : point; b_max : point } [@@marshal]

  (** Get the bounding box from a list of points *)
  val of_points : point list -> t

  (** Get the diagonal vector from the given box *)
  val vec : t -> point

  (** Expand a box by a padding *)
  val expand : float -> t -> t
end

module Matrix3 : sig
  (** The type of 3×3 matrices *)
  type t = { m_a : float; m_b : float; m_c : float;
             m_d : float; m_e : float; m_f : float;
             m_g : float; m_h : float; m_i : float } [@@marshal]

  (** Build a matrix from a translation vector *)
  val of_translation : point -> t

  (** Build a matrix from a scaling factor *)
  val of_scale : float -> t

  (** Apply a matrix to a point *)
  val apply_p2 : point -> t -> point

  (** Apply a matrix to a vector *)
  val apply_v2 : point -> t -> point

  (** Apply a matrix to a segment *)
  val apply_s2 : Segment2.t -> t -> Segment2.t

  (** Multiply two matrices *)
  val mul : t -> t -> t
end

module Vector : sig
  (** The type of n-dimensional vectors *)
  type t = float array

  (** Vector dot product *)
  val dot : float array -> float array -> float

  (** v.v'^n dot product *)
  val dot_1n : float -> float array -> float array -> float

  (** v^n.v'^n dot product *)
  val dot_n : float -> float array -> float array -> float

  (** Add a vector to a point *)
  val plus : float array -> float array -> float array

  (** Subtract a vector from a point *)
  val minus : float array -> float array -> float array
end

module Matrix : sig
  (** The type of n×n matrices *)
  type t = Vector.t array

  (** Build a matrix from a list of vectors *)
  val of_vectors : 'a list -> 'a array

  (** Transpose a matrix *)
  val transpose : 'a array array -> 'a array array
end

(** Conjugate gradient solve for SPD matrix *)
val cg_solve : float array array -> float array -> float array

(** Gradient descent *)
val gd : f:(float array -> float * float array) -> float array -> int -> float array
