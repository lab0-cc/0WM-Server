(** This module implements the physics simulator *)

(** The type of simulator configurations *)
type config = {
  exponent: float;        (** Power decay exponent. Should be 2.0 in free space. *)
  precision: float;       (** Measurement precision in dB *)
  norm: float;            (** The p-norm to use. 1.0 for additive mode, p >> 1.0 for best-AP mode *)
  blend_sharpness: float; (** How sharp the sample blending is *)
}

(** The type of models *)
type model = {
  calibration : Linalg.Vector.t; (** Calibration weights *)
  pts : Linalg.point array;      (** Sample locations *)
  cfg : config;                  (** Simulator configuration *)
  walls : Types.wall list;       (** List of walls *)
  aps : Types.ap list;           (** List of APs *)
  kernel : Linalg.Matrix.t;      (** Kernel matrix *)
  ell : float;                   (** Correlation distance *)
  freq: float                    (** Carrier frequency in Hz *)
}

(** Build the model predictor *)
val build_predictor : cfg:config -> Types.env -> Linalg.point -> float * float
