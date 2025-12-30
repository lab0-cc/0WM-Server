(** This module tests {!Zwmlib.Heap} *)

open Alcotest
open Alcotest_lwt
open Zwmlib.Heap

(** Test for {!Zwmlib.Heap.Make (Zwmlib.Heap.Min (Int))} *)
let test_min () =
  let open Make (Min (Int)) in
  let h = Empty in
  size h |> check int "empty" 0;
  check_raises "empty" (Invalid_argument "bound") (fun () -> bound h |> ignore);
  check_raises "empty" (Invalid_argument "pop") (fun () -> pop h |> ignore);
  let h = add 1 "a" h in
  size h |> check int "add empty" 1;
  let h = add 2 "b" h in
  size h |> check int "add 1" 2;
  let h = add 3 "c" h in
  size h |> check int "add 2" 3;
  let (v, h) = pop h in
  check int "pop 3" 1 v.value;
  check string "pop 3" "a" v.content;
  size h |> check int "pop 3" 2;
  bound h |> check int "bound 2" 2;
  let h = add 4 "d" h in
  size h |> check int "add 2" 3;
  bound h |> check int "bound 3" 2;
  let h = drop h in
  size h |> check int "drop 3" 2;
  bound h |> check int "bound 2" 3;
  let h = add (-1) "z" h in
  check bool "sort" true (sort h = [{ value = 4; content = "d" }; { value = 3; content = "c" };
                                    { value = -1; content = "z" }])

(** Test for {!Zwmlib.Heap.Make (Zwmlib.Heap.Max (Int))} *)
let test_max () =
  let open Make (Max (Int)) in
  let h = Empty in
  size h |> check int "empty" 0;
  let h = add 1 "a" h in
  size h |> check int "add empty" 1;
  let h = add 2 "b" h in
  size h |> check int "add 1" 2;
  let h = add 3 "c" h in
  size h |> check int "add 2" 3;
  let (v, h) = pop h in
  check int "pop 3" 3 v.value;
  check string "pop 3" "c" v.content;
  size h |> check int "pop 3" 2;
  bound h |> check int "bound 2" 2;
  let h = add 4 "d" h in
  size h |> check int "add 2" 3;
  bound h |> check int "bound 3" 4;
  let h = drop h in
  size h |> check int "drop 3" 2;
  bound h |> check int "bound 2" 2;
  let h = add (-1) "z" h in
  check bool "sort" true (sort h = [{ value = -1; content = "z" }; { value = 1; content = "a" };
                                    { value = 2; content = "b" }])

let suite = [
  test_case_sync "test_min" `Quick test_min;
  test_case_sync "test_max" `Quick test_max;
]
