(** This module tests {!Zwmlib.Rtree} *)

open Alcotest
open Alcotest_lwt
open Lwt.Syntax
open Zwmlib.Rtree
open Zwmlib.Store

let push ?tree name anchors structure s =
  let o = { zmin = 0.; zmax = 3.; path = ""; anchors; height = 1.; width = 1.; structure;
            walls = []; name } in
  let* () = push_object name o s in
  match tree with
  | Some tree -> add name tree s
  | None -> singleton name |> Lwt.return

let drop ~tree name s = match%lwt remove name tree s with
  | None -> failwith "Unreachable"
  | Some tree -> Lwt.return tree

let anchor x y lng lat =
  let v = `O [("x", `Float x); ("y", `Float y); ("lng", `Float lng); ("lat", `Float lat)] in
  [%unmarshal.Json] ~v Zwmlib.Anchor.t

let anchor_structure_at long lat =
  let open Zwmlib.Geo in
  let anchors = (anchor 0. 0. long lat, anchor 1. 0. (long +. 1.) lat,
                 anchor 0. 1. long (lat +. 1.)) in
  (anchors, Polygon [{ long; lat }; { long = long +. 1.; lat };
                     { long = long +. 1.; lat = lat +. 1. }; { long; lat = lat +. 1. }])

(** Test for {!Zwmlib.Rtree} *)
let test_rtree switch () =
  let dir = Util.temp_dir () in
  (fun () -> try Util.rm_rf dir |> Lwt.return with _ -> Lwt.return_unit)
  |> Lwt_switch.add_hook (Some switch);
  let* store = Irmin_git.config ~bare:true dir |> Backend.Repo.v in
  let s = Some store |> ref in
  let (anchor, structure) = anchor_structure_at 0. 0. in
  let* tree = push "a" anchor structure s in
  let* structure' = geo_of s tree in
  check bool "geo_of" true (structure' = structure);

  let (anchor, structure) = anchor_structure_at 2. 0. in
  let* tree = push ~tree "b" anchor structure s in
  let bb = [%decode.Json] ~v:{|{"sw":{"lat":0,"long":0},"ne":{"lat":1,"long":3}}|} Zwmlib.Geo.box in
  let* structure = geo_of s tree in
  check bool "geo_of" true (structure = Zwmlib.Geo.Bounding_box bb);
  to_list tree |> List.sort String.compare |> check (list string) "to_list" ["a"; "b"];
  let* distances = sort { long = 0.5; lat = 0.25 } tree s in
  let dists = [(0., "a"); (166791.032, "b")] in
  check (list (pair Util.f3 string)) "sort" dists distances;

  let (anchor, structure) = anchor_structure_at 0. 2. in
  let* tree = push ~tree "c" anchor structure s in
  let bb = [%decode.Json] ~v:{|{"sw":{"lat":0,"long":0},"ne":{"lat":3,"long":3}}|} Zwmlib.Geo.box in
  let* structure = geo_of s tree in
  check bool "geo_of" true (structure = Zwmlib.Geo.Bounding_box bb);
  to_list tree |> List.sort String.compare |> check (list string) "to_list" ["a"; "b"; "c"];
  let* distances = sort ~limit:2 { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort" dists distances;
  let* distances = sort { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort" [(0., "a"); (166791.032, "b"); (194591.390, "c")]
        distances;

  let (anchor, structure) = anchor_structure_at 4. 2. in
  let* tree = push ~tree "d" anchor structure s in
  let bb = [%decode.Json] ~v:{|{"sw":{"lat":0,"long":0},"ne":{"lat":3,"long":5}}|} Zwmlib.Geo.box in
  let* structure = geo_of s tree in
  check bool "geo_of" true (structure = Zwmlib.Geo.Bounding_box bb);
  to_list tree |> List.sort String.compare |> check (list string) "to_list" ["a"; "b"; "c"; "d"];
  let* distances = sort ~limit:2 { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort" dists distances;
  let* distances = sort { long = 0.5; lat = 0.25 } tree s in
  let dists' = [(0., "a"); (166791.032, "b"); (194591.390, "c"); (435038.912, "d")] in
  check (list (pair Util.f3 string)) "sort" dists' distances;

  let (anchor, structure) = anchor_structure_at 0. 5. in
  let* tree = push ~tree "e" anchor structure s in
  let (anchor, structure) = anchor_structure_at 6. 0. in
  let* tree = push ~tree "f" anchor structure s in
  let bb = [%decode.Json] ~v:{|{"sw":{"lat":0,"long":0},"ne":{"lat":6,"long":7}}|} Zwmlib.Geo.box in
  let* structure = geo_of s tree in
  check bool "geo_of" true (structure = Zwmlib.Geo.Bounding_box bb);
  to_list tree |> List.sort String.compare
  |> check (list string) "to_list" ["a"; "b"; "c"; "d"; "e"; "f"];
  let* distances = sort ~limit:2 { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort" dists distances;
  let* distances = sort ~limit:4 { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort" dists' distances;

  let* tree = drop ~tree "a" s in
  to_list tree |> List.sort String.compare
  |> check (list string) "to_list" ["b"; "c"; "d"; "e"; "f"];

  let* tree = drop ~tree "d" s in
  let bb = [%decode.Json] ~v:{|{"sw":{"lat":0,"long":0},"ne":{"lat":6,"long":7}}|} Zwmlib.Geo.box in
  let* structure = geo_of s tree in
  check bool "geo_of" true (structure = Zwmlib.Geo.Bounding_box bb);
  to_list tree |> List.sort String.compare |> check (list string) "to_list" ["b"; "c"; "e"; "f"];
  let* distances = sort { long = 0.5; lat = 0.25 } tree s in
  check (list (pair Util.f3 string)) "sort"
        [(166791.032, "b"); (194591.390, "c"); (528176.629, "e"); (611567.112, "f")] distances;

  let* tree = drop ~tree "b" s in
  let* tree = drop ~tree "c" s in
  let* tree = drop ~tree "e" s in
  let* structure = geo_of s tree in
  let structure' = Zwmlib.Geo.Polygon [{ long = 6.; lat = 0. }; { long = 7.; lat = 0. };
                                       { long = 7.; lat = 1. }; { long = 6.; lat = 1. }] in
  check bool "geo_of" true (structure = structure');
  to_list tree |> List.sort String.compare |> check (list string) "to_list" ["f"];

  let* tree = remove "f" tree s in
  check bool "remove" true (tree = None);

  Lwt.return_unit

let suite = [test_case "test_rtree" `Quick test_rtree]
