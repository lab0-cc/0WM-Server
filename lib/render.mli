(** This module implements heatmap rendering *)

(** The type of renderer configs *)
type config = {
  resolution : float; (** The cell resolution in meters *)
  padding : float; (** The additional padding in meters to compute around the map *)
  scale : float; (** Pixels per meter *)
  steps : int; (** Power level steps for the marching squares algorithm *)
  vmin : float; (** Minimum power level in dBm *)
  vmax : float; (** Maximum power level in dBm *)
  minimal : bool; (** Whether to remove optional indicators *)
}

(** Render the heatmap to SVG *)
val render : cfg:config -> f:(Linalg.point -> float * float) -> Types.env -> string * Linalg.Box2.t
