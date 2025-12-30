(** This module tests {!Zwmlib.Geo} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Geo

let pts = [{ long = -1.1; lat = -0.5 }; { long = 0.5; lat = 1.1 }; { long = 1.1; lat = -1.1 }]
let p = Polygon pts
let mp = Multi_polygon [[{ long = 0.; lat = 0. }; { long = 0.2; lat = 0. };
                         { long = 0.; lat = 0.2 }]; pts]

(** Test for {!Zwmlib.Geo.ll_of_xy} *)
let test_ll_of_xy () =
  let { long; lat } = ll_of_xy Zwmlib.Linalg.{ p_x = 12.34; p_y = 56.78 } in
  check Util.f8 "lng" long 12.34;
  check Util.f8 "lat" lat 56.78

(** Test for {!Zwmlib.Geo.bounding_box} *)
let test_bounding_box () =
  let e = Invalid_argument "bounding_box" in
  check_raises "empty polygon" e (fun () -> bounding_box (Polygon []) |> ignore);
  let bb = {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.1,"long":1.1}}|} in
  let v = bounding_box p in
  [%encode.Json] ~v box |> check string "polygon" bb;
  check_raises "empty multipolygon" e (fun () -> bounding_box (Multi_polygon []) |> ignore);
  let v = bounding_box mp in
  [%encode.Json] ~v box |> check string "multipolygon" bb

(** Test for {!Zwmlib.Geo.merge_pure_bounding_boxes} *)
let test_merge_pure_bounding_boxes () =
  let v = {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.1,"long":1.1}}|} in
  let bb = [%decode.Json] ~v box in
  let v = {|{"sw":{"lat":-0.9,"long":1.9},"ne":{"lat":1.3,"long":3.1}}|} in
  let bb' = [%decode.Json] ~v box in
  let v = merge_pure_bounding_boxes bb bb' in
  [%encode.Json] ~v box
  |> check string "bb" {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.3,"long":3.1}}|}

(** Test for {!Zwmlib.Geo.merge_bounding_boxes} *)
let test_merge_bounding_boxes () =
  let v = {|{"sw":{"lat":-0.9,"long":1.9},"ne":{"lat":1.3,"long":3.1}}|} in
  let bb = [%decode.Json] ~v box in
  let v = merge_bounding_boxes [Bounding_box bb; p] in
  [%encode.Json] ~v box
  |> check string "bb" {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.3,"long":3.1}}|}

(** Test for {!Zwmlib.Geo.obj_distance} *)
let test_obj_distance () =
  let v = {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.1,"long":1.1}}|} in
  let bb = Bounding_box ([%decode.Json] ~v box) in
  let pt = { long = 0.7; lat = -0.5 } in
  let pt' = { long = 0.1; lat = 0.05 } in
  let pt'' = { long = 2.; lat = 2. } in
  obj_distance pt bb |> check Util.f8 "in bb" 0.;
  obj_distance pt' bb |> check Util.f8 "in bb" 0.;
  obj_distance pt'' bb |> check Util.f3 "out bb" 141501.611;
  obj_distance pt p |> check Util.f8 "in polygon" 0.;
  obj_distance pt' p |> check Util.f8 "in polygon" 0.;
  obj_distance pt'' p |> check Util.f3 "out polygon" 194458.143;
  obj_distance pt mp |> check Util.f8 "in multipolygon" 0.;
  obj_distance pt' mp |> check Util.f3 "out multipolygon" 3931.339;
  obj_distance pt'' mp |> check Util.f3 "out multipolygon" 194458.143

(** Test for {!Zwmlib.Geo.box_area} *)
let test_box_area () =
  let v = {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.1,"long":1.1}}|} in
  [%decode.Json] ~v box |> box_area |> check Util.f8 "bb" 4.84

(** Test for {!Zwmlib.Geo.segments} *)
let test_segments () =
  let open Zwmlib.Linalg in
  let open Segment2 in
  let v = {|{"sw":{"lat":-1.1,"long":-1.1},"ne":{"lat":1.1,"long":1.1}}|} in
  let bb = Bounding_box ([%decode.Json] ~v box) in
  let pt = { p_x = -1.1; p_y = -1.1 } in
  let pt' = { p_x = -1.1; p_y = 1.1 } in
  let pt'' = { p_x = 1.1; p_y = 1.1 } in
  let pt''' = { p_x = 1.1; p_y = -1.1 } in
  let segs = [{ s_start = pt; s_end = pt' }; { s_start = pt'; s_end = pt'' };
              { s_start = pt''; s_end = pt''' }; { s_start = pt'''; s_end = pt }] in
  check bool "bb" true (segments bb = segs);
  let pt = { p_x = -1.1; p_y = -0.5 } in
  let pt' = { p_x = 0.5; p_y = 1.1 } in
  let pt'' = { p_x = 1.1; p_y = -1.1 } in
  let segs = [{ s_start = pt'; s_end = pt'' }; { s_start = pt; s_end = pt' };
              { s_start = pt''; s_end = pt }] in
  check bool "polygon" true (segments p = segs);
  let pt = { p_x = 0.; p_y = 0. } in
  let pt' = { p_x = 0.2; p_y = 0. } in
  let pt'' = { p_x = 0.; p_y = 0.2 } in
  let segs = { s_start = pt'; s_end = pt'' }::{ s_start = pt; s_end = pt' }::
             { s_start = pt''; s_end = pt }::segs in
  check bool "multipolygon" true (segments mp = segs)

let suite = [
  test_case_sync "test_ll_of_xy" `Quick test_ll_of_xy;
  test_case_sync "test_bounding_box" `Quick test_bounding_box;
  test_case_sync "test_merge_pure_bounding_boxes" `Quick test_merge_pure_bounding_boxes;
  test_case_sync "test_merge_bounding_boxes" `Quick test_merge_bounding_boxes;
  test_case_sync "test_obj_distance" `Quick test_obj_distance;
  test_case_sync "test_box_area" `Quick test_box_area;
  test_case_sync "test_segments" `Quick test_segments;
]
