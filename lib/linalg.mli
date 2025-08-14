(** This module implements linear algebra primitives *)

(** The type of 2D points *)
type point = { p_x: float; p_y: float } [@@marshal]

(** The type of 3D points *)
type point3 = { p3_x: float; p3_y: float; p3_z: float } [@@marshal]

(** The type of vectors *)
type vector = { v_x: float; v_y: float }

(** The type of matrices *)
type matrix = { m_a: float; m_b: float; m_c: float; m_d: float }

(** The type of segments *)
type segment

(** Create a vector from two points *)
val vec : point -> point -> vector

(** Compute the cross product of two vectors *)
val det : vector -> vector -> float

(** Compute the scalar product of two vectors *)
val dot : vector -> vector -> float

(** Compute the norm of a vector *)
val norm : vector -> float

(** Compute the application of a matrix on a point *)
val app : matrix -> point -> point

(** Translate a point by a vector *)
val tran : vector -> point -> point

(** Scale a vector by a factor *)
val scale : vector -> float -> vector

(** Create a segment from two points *)
val seg : point -> point -> segment

(** Find a point’s closest point on a segment *)
val psclosest : point -> segment -> point
