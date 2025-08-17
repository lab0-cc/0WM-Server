open Zwmlib
open Types
open Linalg

let json = Dream.json ~headers:[("Access-Control-Allow-Origin", "*")]
let html = Dream.html ~headers:[("Access-Control-Allow-Origin", "*")]

let get_maps ?latitude ?longitude ?altitude ?(accuracy=0.) ?(altitude_accuracy=0.) ?(limit=10) () =
  match !Storage.rtree, latitude, longitude with
  | None, _, _ -> [%encode.Json] Gendarme.empty_list |> json
  | tree, None, _ | tree, _, None ->
      [%encode.Json] ?v:(Option.map Rtree.to_list tree) Gendarme.(list string) |> json
  | Some rtree, Some lat, Some long ->
      Rtree.sort ~limit (Geo.{ lat; long }) rtree Storage.store
      |> List.map (fun (m_dst, m_nam) ->
        let obj = Object_store.find m_nam Storage.store in
        let m_cfd =
          if m_dst <= accuracy
          then match altitude with
            | None -> Confidence.Valid2D
            | Some alt ->
                if alt +. altitude_accuracy >= obj.zmin && alt -. altitude_accuracy < obj.zmax
                then Valid3D
                else Valid2D
          else Invalid in
        { m_dst; m_cfd; m_nam })
      |> List.sort
           (fun { m_dst; m_cfd; _ } { m_dst = m_dst'; m_cfd = m_cfd'; _ } ->
             match Confidence.compare m_cfd m_cfd' with
             | 0 -> compare m_dst m_dst'
             | i -> i)
      |> fun v -> [%encode.Json] ~v (Gendarme.list map) |> json

let get_map id =
  let v = Object_store.find id Storage.store in
  [%encode.Json] ~v Object_store.obj |> json

let process_shapes m d =
  let rec process_shapes_rec acc = function
    | []::_ | (_::[])::_ -> failwith "process_shapes"
    | (_::_::[])::tl -> process_shapes_rec acc tl
    | shape::tl ->
        process_shapes_rec (List.map (fun p -> app m p |> tran d |> Geo.ll_of_xy) shape::acc) tl
    | [] -> acc in
  process_shapes_rec []

let push_map { anchors = (a, a', a'' as anchors); shapes; floorplan = { data; width; height };
               zmin; zmax; _ } =
  let ((src, dst), (src', dst'), (src'', dst'')) =
    Anchor.(to_points a, to_points a', to_points a'') in
  let id = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
  let path = Image.(of_base64 data |> save id) in
  let src_v = vec src src' in
  let src_v' = vec src src'' in
  let dst_v = vec dst dst' in
  let dst_v' = vec dst dst'' in
  let det = det src_v src_v' in
  if Float.abs det < 1e-12
  then failwith "The given anchors are colinear";
  let m = { m_a = (dst_v.v_x *. src_v'.v_y -. dst_v'.v_x *. src_v.v_y) /. det;
            m_b = (dst_v'.v_x *. src_v.v_x -. dst_v.v_x *. src_v'.v_x) /. det;
            m_c = (dst_v.v_y *. src_v'.v_y -. dst_v'.v_y *. src_v.v_y) /. det;
            m_d = (dst_v'.v_y *. src_v.v_x -. dst_v.v_y *. src_v'.v_x) /. det } in
  let d = { v_x = dst.p_x -. m.m_a *. src.p_x -. m.m_b *. src.p_y;
            v_y = dst.p_y -. m.m_c *. src.p_x -. m.m_d *. src.p_y } in
  let shape = match process_shapes m d shapes with
    | shape::[] -> Geo.Polygon shape
    | shapes -> Multi_polygon shapes in
  Object_store.(push id { zmin; zmax; path; anchors; shape; width; height } Storage.store);
  begin match !Storage.rtree with
  | None -> Storage.rtree := Some (Rtree.singleton id)
  | Some r -> Storage.rtree := Some (Rtree.add id r Storage.store)
  end;
  html "ok"
