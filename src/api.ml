module U = Util
open Lwt.Syntax
open Lwt.Infix
open Types
open Zwmlib
open Linalg
open Types

let rtree_mutex = Mutex.create ()

let get_map_s id = Store.find_object id Runtime.store

let get_map id = Store.find_object_j id Runtime.store >>= U.json

let delete_map id =
  let* map = Store.find_object id Runtime.store in
  let* () = Store.remove_object id Runtime.store in
  Mutex.lock rtree_mutex;
  let* () = match !Runtime.rtree with
  | None -> Lwt.return_unit
  | Some r ->
      let* rtree = Rtree.remove id r Runtime.store in
      Lwt.return (Runtime.rtree := rtree) in
  Mutex.unlock rtree_mutex;
  Sys.remove map.path;
  let ext = Filename.extension map.path in
  Filename.remove_extension map.path ^ "_thumb" ^ ext |> Sys.remove;
  U.ok ()

let get_heatmap_s ~ssids id =
  let t = Hashtbl.create 10 in
  let* { meta; data } = Store.find_scan id Runtime.store in
  List.iter (fun (_, Store.{ position = { p3_x; p3_z; _ }; measurements; _ }) ->
    List.iter (fun Dot11.{ ap; signal } -> match ap.ssid with
      | Some ssid when List.mem ssid ssids ->
          let f = Dot11.center_freq ap in
          let l = Hashtbl.find_opt t f |> Option.value ~default:[] in
          Hashtbl.replace t (Dot11.center_freq ap)
                          ({ s_p = { p_x = p3_x; p_y = p3_z }; s_dbm = float signal }::l)
      | _ -> ()
    ) measurements) data;
  let spectrum = Hashtbl.fold
                   (fun f s_samples acc -> { s_freq = float f; s_samples; s_aps = [] }::acc) t [] in
  let* walls = match meta with
    | Some meta ->
        let* Store.{ structure; walls; _ } = get_map_s meta.map in
        Geo.segments structure @ walls
        |> List.map (fun s -> { w_seg = Matrix3.apply_s2 s meta.transform; w_tran = 0.6;
                                w_refl = 0.3 })
        |> Lwt.return
    | None -> Lwt.return [] in
  let env = { walls; spectrum } in
  let render_cfg = Render.{ resolution = 0.2; padding = 5.; scale = 40.0; steps = 0; vmin = -90.;
                            vmax = -30.; minimal = true } in
  let sim_cfg = Sim.{ exponent = 2.0; precision = 0.25; norm = 10.; blend_sharpness = 2. } in
  let f = Sim.build_predictor ~cfg:sim_cfg env in
  Render.render ~cfg:render_cfg ~f env |> Lwt.return

let get_heatmap ~ssids id = let* (img, _) = get_heatmap_s ~ssids id in U.svg img

let format_map_sl recurse v =
  let* res =
    if recurse
    then
      let* v = Lwt_list.map_p (fun id -> let* map = get_map_s id in Lwt.return (id, map)) v
               >|= List.sort (fun (_, Store.{ name; _ }) (_, { name = name'; _ }) ->
                     compare name name') in
      [%encode.Json] ~v Gendarme.(map string Store.obj) |> Lwt.return
    else [%encode.Json] ~v Gendarme.(list string) |> Lwt.return in
  U.json res

let get_maps ?latitude ?longitude ?altitude ?(accuracy=0.) ?(altitude_accuracy=0.) ?(limit=1000)
             recurse () = match !Runtime.rtree, latitude, longitude with
  | None, _, _ when recurse -> [%encode.Json] ~v:[] Gendarme.(map string string) |> U.json
  | None, _, _ -> [%encode.Json] Gendarme.empty_list |> U.json
  | Some rtree, None, _ | Some rtree, _, None -> Rtree.to_list rtree |> format_map_sl recurse
  | Some rtree, Some lat, Some long ->
      let* maps = Rtree.sort ~limit (Geo.{ lat; long }) rtree Runtime.store
        >>= Lwt_list.map_p (fun (distance, id) ->
          let* obj = get_map_s id in
          let confidence =
            if distance <= accuracy
            then match altitude with
              | Some alt when alt +. altitude_accuracy >= obj.zmin
                              && alt -. altitude_accuracy < obj.zmax -> Confidence.Valid3D
              | _ -> Valid2D
            else Invalid in
          Lwt.return (id, obj, distance, confidence))
        >|= List.sort
             (fun (_, _, distance, confidence) (_, _, distance', confidence') ->
               match Confidence.compare confidence confidence' with
               | 0 -> compare distance distance'
               | i -> i) in
      if recurse
      then
        let v = List.map (fun (id, mr_map, mr_dst, mr_cfd) -> (id, { mr_dst; mr_cfd; mr_map }))
                         maps in
        [%encode.Json] ~v (Gendarme.map Gendarme.string map_rec) |> U.json
      else
        let v = List.map (fun (id, _, m_dst, m_cfd) -> (id, { m_dst; m_cfd })) maps in
        [%encode.Json] ~v (Gendarme.map Gendarme.string map) |> U.json

let process_shapes m =
  let rec process_shapes_rec acc = function
    | []::_ | (_::[])::_ -> failwith "process_shapes"
    | (_::_::[])::tl -> process_shapes_rec acc tl
    | shape::tl ->
        process_shapes_rec (List.map (fun p -> Matrix3.apply_p2 p m |> Geo.ll_of_xy) shape::acc) tl
    | [] -> acc in
  process_shapes_rec []

let push_to_rtree id =
  Mutex.lock rtree_mutex;
  let* () = match !Runtime.rtree with
  | None -> Lwt.return (Runtime.rtree := Some (Rtree.singleton id))
  | Some r ->
      let* rtree = Rtree.add id r Runtime.store in
      Lwt.return (Runtime.rtree := Some rtree) in
  Mutex.unlock rtree_mutex |> Lwt.return

let push_map { anchors = (a, a', a'' as anchors); structure; walls;
               floorplan = { data; width; height }; zmin; zmax; name } =
  let ((src, dst), (src', dst'), (src'', dst'')) =
    Anchor.(to_points a, to_points a', to_points a'') in
  let id = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
  let path = Image.(of_base64 data |> save ~path:(Runtime.var "data") id) in
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
  let structure = match process_shapes m structure with
    | shape::[] -> Geo.Polygon shape
    | shapes -> Multi_polygon shapes in
  let walls = process_shapes m walls |> List.map (function
    | hd::hd'::[] -> Segment2.of_points (Geo.xy_of_ll hd) (Geo.xy_of_ll hd')
    | _ -> failwith "Irregular wall") in
  let* () = Store.(push_object id { zmin; zmax; path; anchors; structure; walls; width; height; name }
                                  Runtime.store) in
  let* () = push_to_rtree id in
  U.ok ()
