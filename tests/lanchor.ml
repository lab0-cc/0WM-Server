(** This module tests {!Zwmlib.Anchor} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Anchor

(** Test for {!Zwmlib.Anchor.to_points} *)
let test_to_points () =
  let v = {|{ "x": 12.34, "y": 56.78, "lng": 1.2, "lat": 3.4 }|} in
  let Zwmlib.Linalg.({ p_x; p_y }, { p_x = lng; p_y = lat }) = [%decode.Json] ~v t |> to_points in
  check Util.f8 "x" p_x 12.34;
  check Util.f8 "y" p_y 56.78;
  check Util.f8 "lng" lng 1.2;
  check Util.f8 "lat" lat 3.4

let suite = [test_case_sync "test_to_points" `Quick test_to_points]
