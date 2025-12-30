(** This module tests {!Zwmlib.Linalg} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Linalg

let pt = { p_x = 0.; p_y = 0. }
let pt' = { p_x = 1.; p_y = 0. }
let pt'' = { p_x = 0.; p_y = 1. }
let pt''' = { p_x = 1.; p_y = 2. }
let pt'''' = { p_x = 1.; p_y = 1. }

(** Test for {!Zwmlib.Linalg.Vector2} *)
let test_vector2 () =
  let open Vector2 in
  let v = of_points pt pt' in
  let v' = of_points pt pt'' in
  let v'' = of_points pt pt''' in
  let v''' = of_points pt' pt'''' in
  cross v v' |> check Util.f8 "cross" 1.;
  cross v v'' |> check Util.f8 "cross" 2.;
  cross v' v''' |> check Util.f8 "cross" 0.;
  let v''' = scaled 2. v'' in
  check Util.f8 "scaled/x" 2. v'''.p_x;
  check Util.f8 "scaled/y" 4. v'''.p_y;
  let v''' = plus v' v'' in
  check Util.f8 "plus/x" 1. v'''.p_x;
  check Util.f8 "plus/y" 3. v'''.p_y;
  let v''' = neg v''' in
  check Util.f8 "neg/x" (-1.) v'''.p_x;
  check Util.f8 "neg/y" (-3.) v'''.p_y

(** Test for {!Zwmlib.Linalg.Point2} *)
let test_point2 () =
  let open Point2 in
  sqdist pt pt' |> check Util.f8 "sqdist" 1.;
  sqdist pt pt''' |> check Util.f8 "sqdist" 5.;
  dist pt pt' |> check Util.f8 "sqdist" 1.;
  dist pt pt''' |> check Util.f8 "sqdist" (sqrt 5.);
  let f = lerp pt pt''' in
  let pt' = f 0. in
  check Util.f8 "lerp |<|/x" 0. pt'.p_x;
  check Util.f8 "lerp |<|/y" 0. pt'.p_y;
  let pt' = f 1. in
  check Util.f8 "lerp |>|/x" 1. pt'.p_x;
  check Util.f8 "lerp |>|/y" 2. pt'.p_y;
  let pt' = f 0.5 in
  check Util.f8 "lerp |.|/x" 0.5 pt'.p_x;
  check Util.f8 "lerp |.|/y" 1. pt'.p_y;
  let pt' = f 1.5 in
  check Util.f8 "lerp | |>/x" 1.5 pt'.p_x;
  check Util.f8 "lerp | |>/y" 3. pt'.p_y

(** Test for {!Zwmlib.Linalg.Segment2} *)
let test_segment2 () =
  let open Segment2 in
  let s = of_points pt pt' in
  let s' = of_points pt pt'' in
  let s'' = of_points pt'' pt''' in
  let s''' = of_points pt' pt'''' in
  check bool "intersect equal" true (intersect s s = Parallel);
  check bool "intersect inside" true (intersect s s' = Inside pt);
  check bool "intersect outside" true (intersect s s'' = Outside);
  check bool "intersect parallel" true (intersect s' s''' = Parallel);
  crosses s s |> check bool "crosses equal" false;
  crosses s s' |> check bool "crosses inside" true;
  crosses s s'' |> check bool "crosses outside" false;
  crosses s' s''' |> check bool "crosses parallel" false;
  let pt' = mirror_p pt s'' in
  check Util.f8 "mirror_p/x" (-1.) pt'.p_x;
  check Util.f8 "mirror_p/y" 1. pt'.p_y;
  let pt' = mirror_p pt'' s in
  check Util.f8 "mirror_p/x" 0. pt'.p_x;
  check Util.f8 "mirror_p/y" (-1.) pt'.p_y;
  check bool "closest" true (closest pt s'' = pt'');
  let pt' = closest { p_x = 0.75; p_y = 0.5 } s'' in
  check Util.f8 "closest/x" 0.125 pt'.p_x;
  check Util.f8 "closest/y" 1.125 pt'.p_y;
  sqdist_p pt s'' |> check Util.f8 "sqdist_p" 1.;
  sqdist_p { p_x = 0.75; p_y = 0.5 } s'' |> check Util.f8 "sqdist_p" 0.78125

(** Test for {!Zwmlib.Linalg.Box2} *)
let test_box2 () =
  let open Box2 in
  check bool "empty" true (of_points [] = { b_min = pt; b_max = pt });
  let b = of_points [pt; pt'''; pt''''] in
  check bool "vec" true (vec b = Vector2.of_points pt pt''');
  let b = expand 1. b in
  let b' = { b_min = { p_x = -1.; p_y = -1. }; b_max = { p_x = 2.; p_y = 3. } } in
  check bool "expand" true (b = b')

(** Test for {!Zwmlib.Linalg.Matrix3} *)
let test_matrix3 () =
  let open Matrix3 in
  let m = { m_a = 1.; m_b = 0.; m_c = 1.;
            m_d = 0.; m_e = 1.; m_f = 2.;
            m_g = 0.; m_h = 0.; m_i = 1. } in
  check bool "of_translation" true (of_translation pt''' = m);
  let m' = { m_a = 2.; m_b = 0.; m_c = 0.;
             m_d = 0.; m_e = 2.; m_f = 0.;
             m_g = 0.; m_h = 0.; m_i = 1. } in
  check bool "of_scale" true (of_scale 2. = m');
  check bool "apply_p2" true (apply_p2 pt' m = Vector2.plus pt' pt''');
  check bool "apply_p2" true (apply_p2 pt' m' = Vector2.scaled 2. pt');
  check bool "apply_v2" true (apply_v2 pt' m = pt');
  check bool "apply_v2" true (apply_v2 pt' m' = Vector2.scaled 2. pt');
  let s = Segment2.of_points pt' pt''' in
  let s' = Segment2.of_points (apply_p2 pt' m) (apply_p2 pt''' m) in
  check bool "apply_s2" true (apply_s2 s m = s');
  let m'' = { m_a = 2.; m_b = 0.; m_c = 1.;
              m_d = 0.; m_e = 2.; m_f = 2.;
              m_g = 0.; m_h = 0.; m_i = 1. } in
  check bool "mul" true (mul m m' = m'')

(** Test for {!Zwmlib.Linalg.Vector} *)
let test_vector () =
  let open Vector in
  let v = [|0.; 1.; 2.|] in
  let v' = [|3.; 4.; 5.|] in
  check Util.f8 "dot" 14. (dot v v');
  check Util.f8 "dot_1n" 66. (dot_1n 2. v v');
  check Util.f8 "dot_n" 116. (dot_n 2. v v');
  check bool "plus" true (plus v v' = [|3.; 5.; 7.|]);
  check bool "minus" true (minus v v' = [|-3.; -3.; -3.|])

(** Test for {!Zwmlib.Linalg.Matrix} *)
let test_matrix () =
  let open Matrix in
  let m = of_vectors [] in
  let m' = of_vectors [[|1.; 0.; 0.|]; [|0.; 1.; 0.|]; [|1.; 2.; 1.|]] in
  let m'' = of_vectors [[|1.; 0.; 1.|]; [|0.; 1.; 2.|]; [|0.; 0.; 1.|]] in
  check bool "transpose empty" true (transpose m = m);
  check bool "transpose" true (transpose m' = m'')

let suite = [
  test_case_sync "test_vector2" `Quick test_vector2;
  test_case_sync "test_point2" `Quick test_point2;
  test_case_sync "test_segment2" `Quick test_segment2;
  test_case_sync "test_box2" `Quick test_box2;
  test_case_sync "test_matrix3" `Quick test_matrix3;
  test_case_sync "test_vector" `Quick test_vector;
  test_case_sync "test_matrix" `Quick test_matrix;
]
