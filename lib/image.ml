(** This module implements image storage primitives *)

(** Exception raised when an image format is not recognized *)
exception Unrecognized_format

(** The type of base64-encoded images *)
type t = Png of string | Jpeg of string

(** Load a base64-encoded image *)
let of_base64 data = match Str.(bounded_split (regexp ",") data 2) with
  | "data:image/png;base64"::data::[] -> Png (Base64.decode_exn data)
  | "data:image/jpeg;base64"::data::[] -> Jpeg (Base64.decode_exn data)
  | _ -> raise Unrecognized_format

(** Save an image and its thumbnail *)
let save ~path id image =
  let prefix = Filename.concat path id in
  let (suffix, data) = match image with
    | Png data -> ("." ^ "png", data)
    | Jpeg data -> ("." ^ "jpeg", data) in
  let path = prefix ^ suffix in
  let oc = open_out_bin path in
  output_string oc data;
  close_out oc;
  try
    let img = match OImages.(load path [] |> tag) with
      | Rgb24 img -> img
      | Rgba32 img -> img#to_rgb24
      | Index8 img -> img#to_rgb24
      | Index16 img -> img#to_rgb24
      | Cmyk32 _ -> raise Unrecognized_format in
    let (w, h) = (img#width, img#height) in
    let scale = min (256. /. float w) (256. /. float h) in
    let new_w = int_of_float (float w *. scale) in
    let new_h = int_of_float (float h *. scale) in
    let thumb = img#resize None new_w new_h in
    thumb#save (prefix ^ "_thumb" ^ suffix) None [];
    path
  with exn -> Sys.remove path; raise exn
