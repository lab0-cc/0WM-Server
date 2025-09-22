open Linalg
open Types

type config = { resolution: float; padding: float; scale: float; steps: int; vmin: float;
                vmax: float; minimal: bool }

(** Chain isosegments into isopolygons *)
let chain_segments ~walls resolution segs =
  let sqtolerance = resolution *. resolution in
  (* Find the next point *)
  let next p =
    let rec next_rec acc = function
      | [] -> None
      | Segment2.{ s_start; s_end }::tl when s_start = p -> Some (s_end, List.rev_append acc tl)
      | { s_start; s_end }::tl when s_end = p -> Some (s_start, List.rev_append acc tl)
      | hd::tl -> next_rec (hd::acc) tl in
    next_rec [] in
  (* Get the wall on which a point lies, if any *)
  let on_wall p =
    let rec on_wall_rec i = function
      | { w_seg; _ }::tl ->
          if Segment2.sqdist_p p w_seg < sqtolerance
          then Some i
          else on_wall_rec (i + 1) tl
      | [] -> None
    in
    on_wall_rec 0 walls in
  (* Build the polygons *)
  let rec polys acc = function
    | [] -> acc
    | Segment2.{ s_start; s_end }::tl ->
        let rec poly first last points rem =
          match next first rem with
          | Some (p, rem2) -> poly p last (first::points) rem2
          | None ->
              (match next last rem with
               | Some (p, rem1) -> poly first p (points @ [last]) rem1
               | None -> (first::(points @ [last]), rem)) in
        let (p, rem) = poly s_start s_end [] tl in
        polys (p::acc) rem in
  List.filter (fun Segment2.{ s_start; s_end } -> match on_wall s_start, on_wall s_end with
    | Some i, Some i' when i = i' -> false
    | _ -> true) segs |> polys []

(** Generate the power grid *)
let generate_grid ~bb ~f resolution x y =
  let v = { p_x = resolution /. 2.; p_y = resolution /. 2. } in
  Util.par_init ~domains:3 ~zero:[||] y (fun j ->
    Array.init x (fun i ->
      let p = { p_x = bb.Box2.b_min.p_x +. float i *. resolution;
                p_y = bb.b_min.p_y +. float j *. resolution } in
      let p' = Vector2.plus p v in
      (p, p', f p')))

(** Generate the heatmap cells *)
let heatmap_cells ~vmin ~vmax size clip grid =
  let b64 = Array.map (fun arr ->
    Array.map (fun (_, _, (power, confidence)) ->
      let color = Math.normalize ~min:vmin ~max:vmax power |> Color.of_scalar in
      { color with a = Math.lerp 179. 255. confidence |> int_of_float }) arr) grid
    |> Png.encode |> Base64.encode_string in
  let clip_path = Printf.sprintf "xywh(0 0 %.3f %.3f)" clip.p_x clip.p_y in
  Svg.image ~clip_path { p_x = 0.; p_y = 0. } size ("data:image/png;base64," ^ b64)

(** Generate the color scale *)
let svg_color_scale grid_size steps levels =
  let bar_w = grid_size.p_x /. 40. in
  let bar_h = grid_size.p_y -. 12. in
  let bar_x = grid_size.p_x +. bar_w in
  let rects = 64 in
  let rect_h = bar_h /. float rects in
  let rects =
    List.init rects (fun i ->
      let fill = Color.(of_scalar (1.0 -. float i /. float (rects - 1)) |> to_hex) in
      Svg.rect ~fill { p_x = bar_x; p_y = float i *. rect_h +. 6. } { p_x = bar_w; p_y = rect_h })
    |> Util.cat in
  let tick_x = bar_x +. bar_w +. 6. in
  let labels =
    Array.mapi (fun i level ->
      let t = 1. -. float i /. float (steps - 1) in
      Printf.sprintf "%.1f dBm" level
      |> Svg.text ~fill:"#000" { p_x = tick_x; p_y = 10. +. bar_h *. t } 12.) levels
    |> Array.to_list |> Util.cat in
  rects ^ "\n" ^ labels

(** Normalize and lerp *)
let nlerp lv (_, p, (v, _)) (_, p', (v', _)) = Math.normalize ~min:v ~max:v' lv |> Point2.lerp p p'

(** Build a list of offsets on which to print power labels *)
let text_offsets = function
  | k when k > 200. ->
      let lim = k -. 100. in
      let rec text_offsets_rec acc k =
        if k >= lim
        then acc
        else text_offsets_rec (k::acc) (k +. 200.) in
      text_offsets_rec [] 100.
  | k when k > 50. -> [(k -. 50.) /. 2.]
  | _ -> []

(** Compute the length of a polygon *)
let poly_len poly =
  let (len, _) = List.fold_left (fun (len, p) p' -> (len +. Point2.dist p p', p'))
                                (0., List.hd poly) (List.tl poly) in
  len

(** Generate iso power data *)
let isodata ~tr ~walls resolution x y grid level =
  let segs = Util.fold_range (fun acc j ->
    Util.fold_range (fun acc i ->
      let (_, _, (v00, _) as c00) = grid.(j).(i) in
      let (_, _, (v10, _) as c10) = grid.(j).(i + 1) in
      let (_, _, (v11, _) as c11) = grid.(j + 1).(i + 1) in
      let (_, _, (v01, _) as c01) = grid.(j + 1).(i) in
      let case = (if v00 >= level then 1 else 0) + (if v10 >= level then 2 else 0)
               + (if v11 >= level then 4 else 0) + (if v01 >= level then 8 else 0) in
      let case =
        if (case = 5 || case = 10) && v00 +. v10 +. v11 +. v01 < 4. *. level
        then 15 - case
        else case in
      match case with
      | 1 | 14 -> Segment2.{ s_start = nlerp level c00 c01; s_end = nlerp level c00 c10 }::acc
      | 2 | 13 -> { s_start = nlerp level c00 c10; s_end = nlerp level c10 c11 }::acc
      | 3 | 12 -> { s_start = nlerp level c00 c01; s_end = nlerp level c10 c11 }::acc
      | 4 | 11 -> { s_start = nlerp level c01 c11; s_end = nlerp level c10 c11 }::acc
      | 5 -> { s_start = nlerp level c00 c01; s_end = nlerp level c10 c11 }
           ::{ s_start = nlerp level c00 c10; s_end = nlerp level c01 c11 }::acc
      | 6 | 9 -> { s_start = nlerp level c00 c10; s_end = nlerp level c01 c11 }::acc
      | 7 | 8 -> { s_start = nlerp level c00 c01; s_end = nlerp level c01 c11 }::acc
      | 10 -> { s_start = nlerp level c00 c01; s_end = nlerp level c10 c11 }
            ::{ s_start = nlerp level c00 c10; s_end = nlerp level c01 c11 }::acc
      | _ -> acc) acc 0 (x - 2)) [] 0 (y - 2) in
  (* Build isopolygons *)
  let polys = chain_segments ~walls resolution segs
              |> List.map (List.map (fun p -> Matrix3.apply_p2 p tr)) in
  let t = Math.normalize ~min:(-90.) ~max:(-30.) level in
  let fill = Color.(lerp (of_scalar t) black 0.1 |> to_hex) in
  List.mapi (fun i poly ->
    let id = Printf.sprintf "level%.3f-%i" level i in
    let path = Svg.path ~id ~stroke:fill ~sw:1. poly in
    let txt = Printf.sprintf "%.1f dBm" level in
    let labels =
      poly_len poly
      |> text_offsets
      |> List.map (fun offset -> Svg.textpath ~id ~fill:"#000c" ~width:50. ~offset 11. txt)
      |> Util.cat in
    path ^ labels) polys |> Util.cat

(** Renderer entry point *)
let render ~cfg ~f { walls; spectrum } =
  let (aps, samples) = List.fold_left (fun (aps, samples) { s_aps; s_samples; _ } ->
                                         List.(rev_append s_aps aps, rev_append s_samples samples))
                                      ([], []) spectrum in
  let bb = List.map (fun { s_p; _ } -> s_p) samples @ List.map (fun { a_p; _ } -> a_p) aps
           @ List.concat_map (fun { w_seg; _ } -> [w_seg.s_start; w_seg.s_end]) walls
           |> Box2.of_points |> Box2.expand cfg.padding in
  let tr = Matrix3.(Vector2.neg bb.b_min |> of_translation |> mul (of_scale cfg.scale)) in
  let sz = Box2.vec bb in

  let stripes = {|<defs>
  <pattern id="bgstripes" patternUnits="userSpaceOnUse"
           width="6" height="6" patternTransform="rotate(45)">
    <rect width="6" height="6" fill="#fff"/>
    <line x1="0" y1="0" x2="0" y2="6" stroke="#888" stroke-width="1"/>
  </pattern>
</defs>|} in
  let bg = Matrix3.apply_v2 sz tr |> Svg.rect ~fill:"url(#bgstripes)" { p_x = 0.; p_y = 0. } in

  let x = sz.p_x /. cfg.resolution +. 0.5 |> int_of_float in
  let y = sz.p_y /. cfg.resolution +. 0.5 |> int_of_float in

  let lvls = Array.init cfg.steps
                        (fun i -> Math.lerp cfg.vmin cfg.vmax (float i /. float (cfg.steps - 1))) in

  let grid = generate_grid ~bb ~f cfg.resolution x y in
  let grid_size = Matrix3.apply_v2 (Box2.vec bb) tr in
  let grid_image_size = Matrix3.apply_v2 (Vector2.scaled cfg.resolution
                                                         { p_x = float x; p_y = float y }) tr in

  let svg_size =
    if cfg.minimal
    then grid_size
    else Vector2.plus grid_size { p_x = 110.; p_y = 0. } in

  Util.(cat [
    Svg.header svg_size;
    stripes;
    bg;
    heatmap_cells ~vmin:cfg.vmin ~vmax:cfg.vmax grid_image_size grid_size grid;
    par_map ~domains:4 ~zero:"" (isodata ~tr ~walls cfg.resolution x y grid) lvls
    |> Array.to_list |> cat;
    List.map (fun s -> Matrix3.apply_p2 s.s_p tr
                       |> Svg.circle ~fill:"#000" ~stroke:"#fff" ~sw:0.7 ~r:3.) samples |> cat;
    List.map (fun a -> Matrix3.apply_p2 a.a_p tr
                       |> Svg.circle ~fill:"#fff" ~stroke:"#000" ~sw:0.7 ~r:3.) aps |> cat;
    List.map (fun w -> Matrix3.apply_s2 w.w_seg tr |> Svg.line ~stroke:"#333" ~sw:2.) walls |> cat;
    if cfg.minimal then "" else svg_color_scale grid_size cfg.steps lvls;
    Svg.footer], bb)
