(** This module tests {!Zwmlib.Color} *)

open Alcotest
open Alcotest_lwt

let c = Zwmlib.Color.{ r = 231; g = 38; b = 122; a = 255 }
let c' = Zwmlib.Color.{ r = 4; g = 171; b = 158; a = 255 }
let c'' = Zwmlib.Color.{ r = 45; g = 171; b = 227; a = 63 }

(** Test for {!Zwmlib.Color.to_hex} *)
let test_to_hex () =
  Zwmlib.Color.to_hex c |> check string "c" "#e7267a";
  Zwmlib.Color.to_hex c' |> check string "c'" "#04ab9e";
  Zwmlib.Color.to_hex c'' |> check string "c''" "#2dabe33f"

(** Test for {!Zwmlib.Color.lerp} *)
let test_lerp () =
  check bool "c/c'" true Zwmlib.Color.(lerp c c' 0.5 = { r = 118; g = 105; b = 140; a = 255 });
  check bool "c'/c''" true Zwmlib.Color.(lerp c' c'' 0.4 = { r = 20; g = 171; b = 186; a = 178 });
  check bool "c''/c" true Zwmlib.Color.(lerp c'' c 0.3 = { r = 101; g = 131; b = 196; a = 121 })

let suite = [
  test_case_sync "test_to_hex" `Quick test_to_hex;
  test_case_sync "test_lerp" `Quick test_lerp;
]
