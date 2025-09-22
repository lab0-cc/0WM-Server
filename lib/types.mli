(** The type of walls *)
type wall = {
  w_seg: Linalg.Segment2.t; (** Segment representing the wall *)
  w_tran: float;            (** Transmission coefficient *)
  w_refl: float             (** Reflection coefficient *)
}

(** The type of measurement samples *)
type sample = {
  s_p: Linalg.point; (** Sample measurement location *)
  s_dbm: float       (** Sample power in dBm *)
}

(** The type of access points *)
type ap = {
  a_p: Linalg.point;  (** AP location *)
  a_dbm: float option (** Optional AP tx power in dBm *)
}

(** The type of spectrum data *)
type spectrum_datum = {
  s_freq: float;          (** Carrier frequency in Hz *)
  s_samples: sample list; (** Measurement samples *)
  s_aps: ap list          (** List of access points *)
}

(** The type of environments to simulate *)
type env = {
  walls: wall list;             (** List of walls *)
  spectrum: spectrum_datum list (** Spectrum information *)
}
