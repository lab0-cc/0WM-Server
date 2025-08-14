type t = Invalid | Valid2D | Valid3D [@@marshal]

let compare a b = match a, b with
  | Valid3D, Valid3D | Valid2D, Valid2D | Invalid, Invalid -> 0
  | Valid3D, _ -> -1
  | _, Valid3D -> 1
  | Invalid, _ -> 1
  | _, Invalid -> -1
