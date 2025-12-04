open Linalg
[%%marshal.load Json]

type point = { lat : float [@json]; long : float [@json] } [@@marshal]
type box = { sw : point [@json]; ne : point [@json] } [@@marshal]
type obj = Bounding_box of box | Polygon of point list | Multi_polygon of point list list
[@@marshal]

let ll_of_xy { p_x; p_y } = { long = p_x; lat = p_y }

let xy_of_ll { long; lat } = { p_x = long; p_y = lat }

let average_earth_radius = 6_371_008.771

let distance { lat; long } { lat = lat'; long = long' } =
  let rad x = x *. Float.pi /. 180. in
  2. *. average_earth_radius
     *. asin (sqrt ((1. -. cos(lat'-.lat |> rad)
                        +. cos(rad lat) *. cos(rad lat') *. (1. -. cos (long'-.long |> rad)))
                    /. 2.))

let is_degenerate { sw; ne } = sw.long > ne.long

let rec bounding_box = function
  | Bounding_box bb -> bb
  | Polygon [] -> failwith "bounding_box"
  | Polygon (hd::tl) ->
      let rec bb_rec' sw ne = function
        | { lat; long }::tl when long < 0. ->
            bb_rec' { sw with lat = min sw.lat lat }
                    { lat = max ne.lat lat; long = max ne.long long } tl
        | { lat; long }::tl ->
            bb_rec' { lat = min sw.lat lat; long = min sw.long long }
                    { ne with lat = max ne.lat lat } tl
        | [] -> { sw; ne } in
      let rec bb_rec sw ne = function
        | { long; _ }::_ as l when Float.abs (long -. sw.long) > 180. -> bb_rec' sw ne l
        | { lat; long }::tl -> bb_rec { lat = min sw.lat lat; long = min sw.long long}
                                      { lat = max ne.lat lat; long = max ne.long long} tl
        | [] -> { sw; ne } in
      bb_rec hd hd tl
  | Multi_polygon [] -> failwith "bounding_box"
  | Multi_polygon (hd::tl) ->
      let bb = Polygon hd |> bounding_box in
      List.fold_left (fun acc p -> Polygon p |> bounding_box |> merge_pure_bounding_boxes acc) bb tl

and merge_pure_bounding_boxes bb bb' = match (bb, bb') with
  | ({ sw; ne } as bb), ({ sw = sw'; ne = ne' } as bb') when is_degenerate bb = is_degenerate bb' ->
      { sw = { lat = min sw.lat sw'.lat; long = min sw.long sw'.long };
        ne = { lat = max ne.lat ne'.lat; long = max ne.long ne'.long } }
  | ({ sw; ne } as bb), { sw = sw'; _ } when is_degenerate bb && sw'.long > 0. ->
      { sw = { lat = min sw.lat sw'.lat; long = min sw.long sw'.long }; ne }
  | ({ sw; ne } as bb), { ne = ne'; _ } when is_degenerate bb ->
      { sw; ne = { lat = max ne.lat ne'.lat; long = max ne.long ne'.long } }
  | bb, bb' -> merge_pure_bounding_boxes bb' bb

let merge_bounding_boxes l =
  List.fold_left (fun bb obj -> bounding_box obj |> merge_pure_bounding_boxes bb)
                 (List.hd l |> bounding_box) (List.tl l)

let normalize l =
  if Polygon l |> bounding_box |> is_degenerate
  then List.map (fun p -> if p.long < 0. then { p with long = p.long +. 360. } else p) l
  else l

let obj_distance p = function
  | Bounding_box ({ sw; ne } as bb) ->
      (* For convenience, we don’t deal with bounding boxes containing poles. And that’s fine! *)
      let lat = max p.lat sw.lat |> min ne.lat in
      (* We handle however the case where we cross the antimeridian *)
      let long =
        if is_degenerate bb
        then if p.long > 0. then max p.long sw.long else min p.long ne.long
        else max p.long sw.long |> min ne.long in
      distance p { lat; long }
  | Polygon l ->
      let l = normalize l in
      let p_xy = xy_of_ll p in
      (* Recursively compute the winding number and the minimal segment distance *)
      let (_, wn, d) = List.fold_left (fun (pt, wn, d) pt' ->
        let pt' = xy_of_ll pt' in
        let d = Segment2.of_points pt pt' |> Segment2.closest p_xy |> ll_of_xy |> distance p
                |> Float.min d in
        if pt.p_y <= p_xy.p_y
        then
          if pt'.p_y > p_xy.p_y && Vector2.(cross (of_points pt pt') (of_points pt p_xy)) > 0.
          then (pt', wn + 1, d)
          else (pt', wn, d)
        else
          if pt'.p_y <= p_xy.p_y
          then
            if Vector2.(cross (of_points pt pt') (of_points pt p_xy)) < 0.
            then (pt', wn - 1, d)
            else (pt', wn, d)
          else (pt', wn, d))
        (List.rev l |> List.hd |> xy_of_ll, 0, infinity) l in
      (* A 0 winding number means the point is outside the polygon *)
      if wn = 0 then d else 0.
  | Multi_polygon _ -> failwith "obj_distance/Multi_polygon: Not implemented"

let area = function
  | Bounding_box ({ sw; ne } as bb) when is_degenerate bb ->
      (360. +. ne.long -. sw.long) *. (ne.lat -. sw.lat)
  | Bounding_box { sw; ne } -> (ne.long -. sw.long) *. (ne.lat -. sw.lat)
  | Polygon _ -> failwith "Not implemented"
  | Multi_polygon _ -> failwith "Not implemented"

let box_area box = Bounding_box box |> area

let rec segments = function
  | Bounding_box { sw; ne } ->
      let sw = xy_of_ll sw in
      let ne = xy_of_ll ne in
      let nw = { p_x = sw.p_x; p_y = ne.p_y } in
      let se = { p_x = ne.p_x; p_y = sw.p_y } in
      Segment2.[of_points sw nw; of_points nw ne; of_points ne se; of_points se sw]
  | Polygon l ->
      let (_, segments) = List.fold_left (fun (pt, acc) pt' ->
        let pt' = xy_of_ll pt' in
        (pt', Segment2.of_points pt pt'::acc)) (List.rev l |> List.hd |> xy_of_ll, []) l in
      segments
  | Multi_polygon l -> List.concat_map (fun p -> segments (Polygon p)) l
