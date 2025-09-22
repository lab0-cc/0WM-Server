(** This module generates SVG strings *)

(** Generate an SVG header *)
val header : Linalg.point -> string

(** Generate an SVG footer *)
val footer : string

(** Generate an SVG <rect> element *)
val rect : ?fill:string -> ?opacity:float -> ?stroke:string -> ?sw:float -> Linalg.point
                        -> Linalg.point -> string

(** Generate an SVG <line> element *)
val line : ?stroke:string -> ?sw:float -> Linalg.Segment2.t -> string

(** Generate an SVG <circle> element *)
val circle : ?fill:string -> ?stroke:string -> ?sw:float -> ?r:float -> Linalg.point -> string

(** Generate an SVG <path> element *)
val path : ?id:string -> ?fill:string -> ?stroke:string -> ?sw:float -> Linalg.point list -> string

(** Generate an SVG <text> element *)
val text : ?fill:string -> ?font:string -> ?anchor:string -> Linalg.point -> float -> string
                        -> string

(** Generate an SVG <textPath> element wrapped within a <text> element *)
val textpath : id:string -> ?fill:string -> ?font:string -> ?width:float -> ?offset:float -> float
                         -> string -> string

(** Generate an SVG <image> element *)
val image : ?clip_path:string -> Linalg.point -> Linalg.point -> string -> string
