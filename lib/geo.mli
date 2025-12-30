(** This module implements geo shapes *)

(** The type of longitude/latitude points *)
type point = { lat : float; long : float } [@@marshal]

(** The type of bounding boxes *)
type box [@@marshal]

(** The type of geo shapes *)
type obj = Bounding_box of box | Polygon of point list | Multi_polygon of point list list
[@@marshal]

(** Convert an x/y point to a longitude/latitude one *)
val ll_of_xy : Linalg.point -> point

(** Convert a longitude/latitude point to an x/y one *)
val xy_of_ll : point -> Linalg.point

(** Compute the bounding box of a shape *)
val bounding_box : obj -> box

(** Merge two bounding boxes *)
val merge_pure_bounding_boxes : box -> box -> box

(** Compute the bounding box of a list of objects *)
val merge_bounding_boxes : obj list -> box

(** Compute the minimum distance between a point and an object *)
val obj_distance : point -> obj -> float

(** Compute a bounding box angular area. This does not compute surfaces. *)
val box_area : box -> float

(** Extract all the segments of a shape *)
val segments : obj -> Linalg.Segment2.t list
