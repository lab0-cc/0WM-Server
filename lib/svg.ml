open Linalg

let header v =
  Printf.sprintf
    {|<svg xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">|}
    v.p_x v.p_y v.p_x v.p_y

let footer = "</svg>\n"

let opt_f s = function
  | Some f -> Printf.sprintf {| %s="%.3f"|} s f
  | None -> ""

let opt_s s = function
  | Some s' -> Printf.sprintf {| %s="%s"|} s s'
  | None -> ""

let rect ?(fill="none") ?opacity ?(stroke="none") ?sw p v =
  Printf.sprintf
    {|<rect x="%.3f" y="%.3f" width="%.3f" height="%.3f" fill="%s" stroke="%s" shape-rendering="optimizeSpeed"%s%s/>|}
    p.p_x p.p_y v.p_x v.p_y fill stroke (opt_f "fill-opacity" opacity) (opt_f "stroke-width" sw)

let line ?(stroke="none") ?sw Segment2.{ s_start; s_end } =
  opt_f "stroke-width" sw
  |> Printf.sprintf {|<line x1="%.3f" y1="%.3f" x2="%.3f" y2="%.3f" stroke="%s"%s/>|}
                    s_start.p_x s_start.p_y s_end.p_x s_end.p_y stroke

let circle ?(fill="none") ?(stroke="none") ?sw ?(r=1.) p =
  opt_f "stroke-width" sw
  |> Printf.sprintf {|<circle cx="%.3f" cy="%.3f" r="%.3f" fill="%s" stroke="%s"%s/>|}
                    p.p_x p.p_y r fill stroke

let path ?id ?(fill="none") ?(stroke="none") ?sw points = match points with
  | [] | _::[] -> ""
  | { p_x; p_y }::tl ->
      let d = List.map (fun { p_x; p_y } -> Printf.sprintf "L %.3f %.3f " p_x p_y) tl
              |> String.concat "" |> Printf.sprintf "M %.3f %.3f %s" p_x p_y in
      let sw = opt_f "stroke-width" sw in
      let id = opt_s "id" id in
      Printf.sprintf {|<path%s d="%s" fill="%s" stroke="%s"%s/>|} id d fill stroke sw

let text ?(fill="none") ?(font="sans-serif") ?(anchor="start") p =
  Printf.sprintf
    {|<text x="%.3f" y="%.3f" fill="%s" text-anchor="%s" font-family="%s" font-size="%.1f">%s</text>|}
    p.p_x p.p_y fill anchor font

let textpath ~id ?(fill="none") ?(font="sans-serif") ?width ?offset =
  let width = opt_f "textLength" width in
  let offset = opt_f "startOffset" offset in
  Printf.sprintf
    {|<text><textPath href="#%s" fill="%s" font-family="%s"%s%s font-size="%.1f">%s</textPath></text>|}
    id fill font width offset

let image ?clip_path p v =
  opt_s "clip-path" clip_path |>
  Printf.sprintf
    {|<image x="%.3f" y="%.3f" width="%.3f" height="%.3f"%s href="%s" preserveAspectRatio="none" image-rendering="optimizeSpeed"/>|}
    p.p_x p.p_y v.p_x v.p_y
