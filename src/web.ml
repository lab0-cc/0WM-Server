open Lwt.Syntax

let get_parameter f request parameter =
  Dream.query request parameter |> Option.map (fun v -> match f v with
    | Some v -> v
    | None -> raise (Util.Bad_parameter parameter))

let get_float = get_parameter Float.of_string_opt

let get_int = get_parameter int_of_string_opt

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

let signal = ref None

let rec server ~error_handler store =
  let stop, resolver = Lwt.wait () in
  signal := Some resolver;
  let* { interface; port; _ } = Zwmlib.Store.get_conf Runtime.store in
  let%lwt () = Dream.serve ~interface ~port ~error_handler ~stop
  (* TODO: use a more specialized logger, see https://github.com/camlworks/dream/issues/413 *)
  @@ Dream.logger
  @@ Dream.router [
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
    Dream.post "/maps" (fun req ->
      let%lwt v = Dream.body req in
      [%decode.Json] ~v Types.payload |> Api.push_map
    );
    Dream.options "/maps" (fun _ -> Dream.respond ~headers:[("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", "*")] ~status:`No_Content "");
    Dream.get "/maps/:id" (fun request -> Dream.param request "id" |> Api.get_map);
    Dream.delete "/maps/:id" (fun request -> Dream.param request "id" |> Api.delete_map);
    Dream.get "/data/**" (Runtime.var "data" |> Dream.static);
    Dream.get "/heatmaps/:id" (fun request ->
      let ssids = Dream.queries request "ssid" in
      Dream.param request "id" |> Api.get_heatmap ~ssids
    );
    Dream.get "/debug/rtree" (fun _ -> [%encode.Json] ~v:!Runtime.rtree (Gendarme.option Zwmlib.Rtree.t) |> Dream.json);
    Dream.get "/debug/store" (fun request -> debug_store ?branch:(Dream.query request "branch") ~store []);
    Dream.get "/debug/store/**" (fun request ->
      (Dream.path [@alert "-deprecated"]) request |> List.filter ((<>) "")
      |> debug_store ?branch:(Dream.query request "branch") ~store);
    Dream.get "/swagger/**" (Runtime.static "swagger" |> Dream.static);
    Dream.get "/" (Dream.from_filesystem (Runtime.static "static") "api.html");
    (* TODO: generate this file *)
    Dream.get "/api.yml" (Dream.from_filesystem (Runtime.static "static") "api.yml");
    Dream.get "/ws" (fun _ -> Dream.websocket Ws.live);
  ] in
  signal := None;
  Dream.info (fun m -> m "Restarting web server");
  server ~error_handler store
