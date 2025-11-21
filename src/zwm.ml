(** This module implements the entrypoint for the 0WM server *)

open Types
open Lwt.Syntax
open Lwt.Infix

let src = Logs.Src.create "zwm.init" ~doc:"0WM initialization routine"
module Log = (val Logs.src_log src : Logs.LOG)

let (>>=) = Option.bind

let get_float request parameter =
  Dream.query request parameter >>= Float.of_string_opt

let get_int request parameter =
  Dream.query request parameter >>= int_of_string_opt

let get_toggle request parameter = match Dream.query request parameter with
  | None -> false
  | Some s -> match String.lowercase_ascii s with
      | "false" | "no" | "disable" | "0" -> false
      | _ -> true

let debug_store ?branch ~store path =
  let* branch = match branch with
    | None | Some "main" -> Runtime.Store.main store
    | Some b -> Runtime.Store.of_branch store b in
  let* data = Runtime.Proj.get branch path in
  Ezjsonm.value_to_string data |> Dream.json

let cleanup_open_sessions store =
  Log.info (fun m -> m "Cleaning up open sessions");
  Lwt_list.iter_s (fun b -> Zwmlib.Store.cleanup_scan (String.sub b 6 36) store)

let rebuild_rtree store =
  Log.info (fun m -> m "Rebuilding R-tree");
  let* main = Runtime.Store.main store in
  let* objects = Runtime.Store.list main ["objects"] in
  Lwt_list.iter_s (fun (o, _) -> Api.update_rtree o) objects

let init_web store =
  Dream.serve ~interface:Config.config.interface ~port:Config.config.port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/data/**" (Dream.static "data");
    Dream.get "/heatmaps/:id" (fun request ->
      let ssids = Dream.queries request "ssid" in
      Dream.param request "id" |> Api.get_heatmap ~ssids
    );
    Dream.get "/maps" (fun request ->
      let latitude = get_float request "latitude" in
      let longitude = get_float request "longitude" in
      let altitude = get_float request "altitude" in
      let accuracy = get_float request "accuracy" in
      let altitude_accuracy = get_float request "altitude-accuracy" in
      let limit = get_int request "limit" in
      let recurse = get_toggle request "recurse" in
      Api.get_maps ?latitude ?longitude ?altitude ?accuracy ?altitude_accuracy ?limit recurse ()
    );
    Dream.get "/maps/:id" (fun request -> Dream.param request "id" |> Api.get_map);
    Dream.post "/maps" (fun req ->
      let%lwt v = Dream.body req in
      [%decode.Json] ~v payload |> Api.push_map
    );
    Dream.options "/maps" (fun _ -> Dream.respond ~headers:[("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", "*")] ~status:`No_Content "");
    Dream.get "/ws" (fun _ -> Dream.websocket Ws.live);
    Dream.get "/debug/rtree" (fun _ -> [%encode.Json] ~v:!Runtime.rtree (Gendarme.option Zwmlib.Rtree.t) |> Dream.json);
    Dream.get "/debug/store" (fun request -> debug_store ?branch:(Dream.query request "branch") ~store []);
    Dream.get "/debug/store/**" (fun request ->
      (Dream.path [@alert "-deprecated"]) request |> List.filter ((<>) "")
      |> debug_store ?branch:(Dream.query request "branch") ~store);
  ]

let init =
  Dream.initialize_log ();
  let* store = Runtime.(Store.Repo.v config) in
  Runtime.store := Some store;
  let* branches = Runtime.Store.Branch.list store >|= List.filter (String.starts_with ~prefix:"scans/") in
  let* () = cleanup_open_sessions Runtime.store branches in
  let* () = rebuild_rtree store in
  Log.info (fun m -> m "Initialization completed");
  init_web store

let () = Lwt_main.run init
