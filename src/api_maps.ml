module U = Util
open Lwt.Infix
open Lwt.Syntax
open Zwmapi.Types
open Zwmlib
open Linalg

(** Process raw shape data *)
let process_shapes m =
  let rec process_shapes_rec acc = function
    | []::_ | (_::[])::_ -> failwith "process_shapes"
    | shape::tl ->
        process_shapes_rec (List.map (fun p -> Matrix3.apply_p2 p m |> Geo.ll_of_xy ~local:p)
                                     shape::acc) tl
    | [] -> acc in
  process_shapes_rec []

(** Get a raw map *)
let get_id_s id = Store.find_object id Runtime.store

(** Format a map or a map thereof *)
let format_maps recurse v = begin
  if recurse
  then
    let* v = Lwt_list.map_p (fun id ->
      let* map = get_id_s id in
      Lwt.return (id, map)
    ) v >|= List.sort (fun (_, Store.{ name; _ }) (_, { name = name'; _ }) -> compare name name') in
    [%encode.Json] ~v Gendarme.(map string Store.obj) |> Lwt.return
  else [%encode.Json] ~v Gendarme.(list string) |> Lwt.return
  end >>= U.json

(** Process a POST/PUT payload *)
let process_payload (a, a', a'') structure walls =
  let ((src, dst), (src', dst'), (src'', dst'')) =
    Anchor.(to_points a, to_points a', to_points a'') in
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
  let walls = List.map (function
    | hd::hd'::[] -> Segment2.of_points hd hd'
    | _ -> failwith "Irregular wall") walls in
  (structure, walls)

(** Get a list of maps around the given position with the given accuracy
    GET /maps *)
let get ?latitude ?longitude ?altitude ?(accuracy=0.) ?(altitude_accuracy=0.) ?(limit=1000) recurse
        () = match !Runtime.rtree, latitude, longitude with
  | None, _, _ when recurse -> [%encode.Json] ~v:[] Gendarme.(map string string) |> U.json
  | None, _, _ -> [%encode.Json] Gendarme.empty_list |> U.json
  | Some rtree, None, _ | Some rtree, _, None -> Rtree.to_list rtree |> format_maps recurse
  | Some rtree, Some lat, Some long ->
      let* maps = Rtree.sort ~limit (Geo.ll long lat) rtree Runtime.store
        >>= Lwt_list.map_p (fun (distance, id) ->
          let* obj = get_id_s id in
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

(** Get the bounding box containing all saved floorplans
    GET /maps/box *)
let get_box () = match !Runtime.rtree with
  | None -> U.json "null"
  | Some r ->
      let%lwt v = Rtree.geo_of Runtime.store r >|= Geo.bounding_box in
      [%encode.Json] ~v Geo.box |> U.json

(** Create a new map in the store
    POST /maps *)
let post { anchors; structure; walls; floorplan = { data; width; height }; zmin; zmax; name } =
  let id = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
  let path = Image.(of_base64 data |> save ~path:(Runtime.var "data") id) in
  let (structure, walls) = process_payload anchors structure walls in
  let* () =
    Store.(push_object id { zmin; zmax; path; anchors; structure; walls; width; height; name }
                       Runtime.store) in
  let* () = Runtime.rtree_push id in
  U.ok ()

(** Delete a map by its ID
    DELETE /maps/:id *)
let delete_id id =
  let* map = get_id_s id in
  let* () = Store.remove_object id Runtime.store in
  Mutex.lock Runtime.rtree_mutex;
  let* () = match !Runtime.rtree with
    | None -> Lwt.return_unit
    | Some r ->
        let* rtree = Rtree.remove id r Runtime.store in
        Lwt.return (Runtime.rtree := rtree) in
  Mutex.unlock Runtime.rtree_mutex;
  Sys.remove map.path;
  let ext = Filename.extension map.path in
  Filename.remove_extension map.path ^ "_thumb" ^ ext |> Sys.remove;
  U.ok ()

(** Get a map by its ID
    GET /maps/:id *)
let get_id id = Store.find_object_j id Runtime.store >>= U.json

(** Update a map
    PUT /maps/:id *)
let put_id id { anchors; structure; walls; zmin; zmax; name; _ } =
  let* { path; width; height; _ } = get_id_s id in
  let (structure, walls) = process_payload anchors structure walls in
  let* () =
    Store.(push_object id { zmin; zmax; path; anchors; structure; walls; width; height; name }
                       Runtime.store) in
  let* () = Runtime.rtree_reinsert id in
  U.ok ()

let endpoints = [
  Dream.get "/maps" (fun request ->
    let latitude = Api.get_float request "latitude" in
    let longitude = Api.get_float request "longitude" in
    let altitude = Api.get_float request "altitude" in
    let accuracy = Api.get_float request "accuracy" in
    let altitude_accuracy = Api.get_float request "altitude-accuracy" in
    let limit = Api.get_int request "limit" in
    let recurse = Api.get_toggle request "recurse" in
    get ?latitude ?longitude ?altitude ?accuracy ?altitude_accuracy ?limit recurse ()
  );
  Dream.post "/maps" (fun request ->
    let%lwt v = Dream.body request in
    [%decode.Json] ~v payload |> post
  );
  Cors.endpoint "/maps";
  Dream.get "/maps/box" (fun _ -> get_box ());
  Dream.delete "/maps/:id" (fun request -> Dream.param request "id" |> delete_id);
  Dream.get "/maps/:id" (fun request -> Dream.param request "id" |> get_id);
  Dream.put "/maps/:id" (fun request ->
    let%lwt v = Dream.body request in
    [%decode.Json] ~v payload |> put_id (Dream.param request "id")
  );
  Cors.endpoint ~methods:[`DELETE; `PUT] "/maps/:id"
]
