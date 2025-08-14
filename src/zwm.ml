(** This module implements the entrypoint for the 0WM server *)

open Zwmlib
open Types
open Linalg

let (>>=) = Option.bind

let get_float request parameter =
  Dream.query request parameter >>= Float.of_string_opt

let get_int request parameter =
  Dream.query request parameter >>= int_of_string_opt

let json = Dream.json ~headers:[("Access-Control-Allow-Origin", "*")]
let html = Dream.html ~headers:[("Access-Control-Allow-Origin", "*")]

let rtree = ref None
let store = Object_store.empty

let get_maps ?latitude ?longitude ?altitude ?(accuracy=0.) ?(altitude_accuracy=0.) ?(limit=10) () =
  match !rtree, latitude, longitude with
  | None, _, _ -> [%encode.Json] Gendarme.empty_list |> json
  | tree, None, _ | tree, _, None ->
      [%encode.Json] ?v:(Option.map Rtree.to_list tree) Gendarme.(list string) |> json
  | Some rtree, Some lat, Some long ->
      Rtree.sort ~limit (Geo.{ lat; long }) rtree store
      |> List.map (fun (m_dst, m_nam) ->
        let obj = Object_store.find m_nam store in
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
  let v = Object_store.find id store in
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
  Object_store.(push id { zmin; zmax; path; anchors; shape; width; height } store);
  begin match !rtree with
  | None -> rtree := Some (Rtree.singleton id)
  | Some r -> rtree := Some (Rtree.add id r store)
  end;
  html "ok"

let () =
  Dream.run ~interface:"127.0.0.1" ~port:8000
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (Dream.from_filesystem "static" "index.html");
    Dream.get "/css/**" (Dream.static "static/css");
    Dream.get "/js/**" (Dream.static "static/js");
    Dream.get "/data/**" (Dream.static "data");
    Dream.get "/api/maps" (fun request ->
      get_maps ?latitude:(get_float request "latitude") ?longitude:(get_float request "longitude")
               ?altitude:(get_float request "altitude") ?accuracy:(get_float request "accuracy")
               ?altitude_accuracy:(get_float request "altitude-accuracy")
               ?limit:(get_int request "limit") ()
    );
    Dream.get "/api/maps/:id" (fun request -> Dream.param request "id" |> get_map);
    Dream.post "/api/maps" (fun req ->
      let%lwt v = Dream.body req in
      [%decode.Json] ~v payload |> push_map
    );
    Dream.options "/api/maps" (fun _ -> Dream.respond ~headers:[("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", "*")] ~status:`No_Content "");
    Dream.get "/debug" (fun _ -> [%encode.Json] ~v:!rtree (Gendarme.option Zwmlib.Rtree.t) |> Dream.json)
  ]
