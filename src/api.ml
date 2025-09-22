open Types
open Zwmlib
open Linalg
open Types

let json = Dream.json ~headers:[("Access-Control-Allow-Origin", "*")]
let html = Dream.html ~headers:[("Access-Control-Allow-Origin", "*")]
let svg s = Dream.response ~headers:[("Content-Type", "image/svg+xml");
                                     ("Access-Control-Allow-Origin", "*")] s |> Lwt.return

let get_heatmap_s ~ssids id =
  let t = Hashtbl.create 10 in
  Hashtbl.find_all Storage.measurements id
  |> List.iter (fun Scan_store.{ position = { p3_x; p3_z; _ }; measurements; _ } ->
       List.iter (fun Dot11.{ ap; signal } -> match ap.ssid with
         | Some ssid when List.mem ssid ssids ->
             let f = Dot11.center_freq ap in
             let l = Hashtbl.find_opt t f |> Option.value ~default:[] in
             Hashtbl.replace t (Dot11.center_freq ap)
                             ({ s_p = { p_x = p3_x; p_y = p3_z }; s_dbm = float signal }::l)
         | _ -> ()
       ) measurements
     );
  let spectrum = Hashtbl.fold
                   (fun f s_samples acc -> { s_freq = float f; s_samples; s_aps = [] }::acc) t [] in
  let env = { walls = []; spectrum } in
  let render_cfg = Render.{ resolution = 0.02; padding = 5.; scale = 40.0; steps = 0; vmin = -90.;
                            vmax = -30.; minimal = true } in
  let sim_cfg = Sim.{ exponent = 2.0; precision = 0.25; norm = 10.; blend_sharpness = 2. } in
  let f = Sim.build_predictor ~cfg:sim_cfg env in
  Render.render ~cfg:render_cfg ~f env

let get_heatmap ~ssids id = let (img, _) = get_heatmap_s ~ssids id in svg img

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

let process_shapes m =
  let rec process_shapes_rec acc = function
    | []::_ | (_::[])::_ -> failwith "process_shapes"
    | (_::_::[])::tl -> process_shapes_rec acc tl
    | shape::tl ->
        process_shapes_rec (List.map (fun p -> Matrix3.apply_p2 p m |> Geo.ll_of_xy) shape::acc) tl
    | [] -> acc in
  process_shapes_rec []

let push_map { anchors = (a, a', a'' as anchors); shapes; floorplan = { data; width; height };
               zmin; zmax; _ } =
  let ((src, dst), (src', dst'), (src'', dst'')) =
    Anchor.(to_points a, to_points a', to_points a'') in
  let id = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
  let path = Image.(of_base64 data |> save id) in
  let src_v = Vector2.of_points src src' in
  let src_v' = Vector2.of_points src src'' in
  let dst_v = Vector2.of_points dst dst' in
  let dst_v' = Vector2.of_points dst dst'' in
  let det = Vector2.cross src_v src_v' in
  if Float.abs det < 1e-12
  then failwith "The given anchors are colinear";
  let m = Matrix3.{ m_a = (dst_v.p_x *. src_v'.p_y -. dst_v'.p_x *. src_v.p_y) /. det;
                    m_b = (dst_v'.p_x *. src_v.p_x -. dst_v.p_x *. src_v'.p_x) /. det;
                    m_c = 0.;
                    m_d = (dst_v.p_y *. src_v'.p_y -. dst_v'.p_y *. src_v.p_y) /. det;
                    m_e = (dst_v'.p_y *. src_v.p_x -. dst_v.p_y *. src_v'.p_x) /. det;
                    m_f = 0.;
                    m_g = 0.; m_h = 0.; m_i = 1. } in
  let src' = Matrix3.apply_v2 src m in
  let m = { m with m_c = dst.p_x -. src'.p_x; m_f = dst.p_y -. src'.p_y } in
  let shape = match process_shapes m shapes with
    | shape::[] -> Geo.Polygon shape
    | shapes -> Multi_polygon shapes in
  Object_store.(push id { zmin; zmax; path; anchors; shape; width; height } Storage.store);
  begin match !Storage.rtree with
  | None -> Storage.rtree := Some (Rtree.singleton id)
  | Some r -> Storage.rtree := Some (Rtree.add id r Storage.store)
  end;
  html "ok"
