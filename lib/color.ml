type t = { r: int; g: int; b: int; a: int }

let black = { r = 0; g = 0; b = 0; a = 255 }

let white = { r = 255; g = 255; b = 255; a = 255 }

let to_hex { r; g; b; a } = match a with
  | 255 -> Printf.sprintf "#%02x%02x%02x" r g b
  | _ -> Printf.sprintf "#%02x%02x%02x%02x" r g b a

let stops =
  [(0.00, { r = 5; g = 48; b = 97; a = 255 });
   (0.25, { r = 33; g = 144; b = 141; a = 255 });
   (0.50, { r = 94; g = 201; b = 98; a = 255 });
   (0.75, { r = 253; g = 231; b = 37; a = 255 });
   (1.00, { r = 244; g = 67; b = 54; a = 255 })]

let lerp c c' s =
  let f a b = Math.lerp (float a) (float b) s +. 0.5 |> int_of_float in
  { r = f c.r c'.r; g = f c.g c'.g; b = f c.b c'.b; a = f c.a c'.a }

let of_scalar s =
  let s = Math.clamp ~min:0. ~max:1. s in
  let rec find (v, c) = function
    | [] -> c
    | (v', c')::_ when s <= v' ->
        let span = v' -. v in
        let a = if span <= Util.eps then 0.0 else (s -. v) /. span in
        lerp c c' a
    | hd::tl -> find hd tl in
  match stops with
  | [] -> { r = 0; g = 0; b = 0; a = 255 }
  | hd::tl -> find hd tl
