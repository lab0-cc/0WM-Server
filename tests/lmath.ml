(** This module tests {!Zwmlib.Math} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Math

(** Test for {!Zwmlib.Math.clamp} *)
let test_clamp () =
  let f = clamp ~min:0 ~max:10 in
  f 5 |> check int "in" 5;
  f (-5) |> check int "out-" 0;
  f 15 |> check int "out+" 10

(** Test for {!Zwmlib.Math.lerp} *)
let test_lerp () =
  let f = lerp 0. 10. in
  f 0. |> check Util.f8 "|<|" 0.;
  f 1. |> check Util.f8 "|>|" 10.;
  f 0.5 |> check Util.f8 "|.|" 5.;
  f 1.5 |> check Util.f8 "| |>" 15.

(** Test for {!Zwmlib.Math.normalize} *)
let test_normalize () =
  let f = normalize ~min:0. ~max:10. in
  f 0. |> check Util.f8 "|<|" 0.;
  f 10. |> check Util.f8 "|>|" 1.;
  f 5. |> check Util.f8 "|.|" 0.5;
  f 15. |> check Util.f8 "| |>" 1.

(** Test for {!Zwmlib.Math.median} *)
let test_median () =
  check_raises "empty" (Invalid_argument "median") (fun () -> median [] |> ignore);
  median [1.; 4.; 3.; 5.; 2.] |> check Util.f8 "odd" 3.;
  median [1.; 4.; 3.; 5.; 2.; 6.] |> check Util.f8 "even" 3.5

(** Test for {!Zwmlib.Math.matern32} *)
let test_matern32 () =
  matern32 0. |> check Util.f8 "1" 1.;
  matern32 1. |> check Util.f3 "1" 0.483;
  matern32 2. |> check Util.f3 "2" 0.140;
  matern32 3. |> check Util.f3 "3" 0.034

let suite = [
  test_case_sync "test_clamp" `Quick test_clamp;
  test_case_sync "test_lerp" `Quick test_lerp;
  test_case_sync "test_normalize" `Quick test_normalize;
  test_case_sync "test_median" `Quick test_median;
  test_case_sync "test_matern32" `Quick test_matern32;
]
