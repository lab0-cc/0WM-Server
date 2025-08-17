(** This module implements the entrypoint for the 0WM server *)

open Types

let (>>=) = Option.bind

let get_float request parameter =
  Dream.query request parameter >>= Float.of_string_opt

let get_int request parameter =
  Dream.query request parameter >>= int_of_string_opt

let () =
  Dream.run ~interface:Config.config.interface ~port:Config.config.port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/data/**" (Dream.static "data");
    Dream.get "/maps" (fun request ->
      let latitude = get_float request "latitude" in
      let longitude = get_float request "longitude" in
      let altitude = get_float request "altitude" in
      let accuracy = get_float request "accuracy" in
      let altitude_accuracy = get_float request "altitude-accuracy" in
      let limit = get_int request "limit" in
      Api.get_maps ?latitude ?longitude ?altitude ?accuracy ?altitude_accuracy ?limit ()
    );
    Dream.get "/maps/:id" (fun request -> Dream.param request "id" |> Api.get_map);
    Dream.post "/maps" (fun req ->
      let%lwt v = Dream.body req in
      [%decode.Json] ~v payload |> Api.push_map
    );
    Dream.options "/maps" (fun _ -> Dream.respond ~headers:[("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", "*")] ~status:`No_Content "");
    Dream.get "/ws" (fun _ -> Dream.websocket Ws.live);
    Dream.get "/debug/rtree" (fun _ -> [%encode.Json] ~v:!Storage.rtree (Gendarme.option Zwmlib.Rtree.t) |> Dream.json);
    Dream.get "/debug/store" (fun _ -> [%encode.Json] ~v:Storage.store (Zwmlib.Object_store.t) |> Dream.json);
    Dream.get "/debug/measurements" (fun _ -> [%encode.Json] ~v:Storage.measurements (Zwmlib.Scan_store.t) |> Dream.json)
  ]
