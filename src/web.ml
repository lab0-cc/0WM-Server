open Lwt.Syntax

let signal = ref None

let rec server ~error_handler store =
  let stop, resolver = Lwt.wait () in
  signal := Some resolver;
  let* { interface; port; _ } = Zwmlib.Store.get_conf Runtime.store in
  let%lwt () = Dream.serve ~interface ~port ~error_handler ~stop
  (* TODO: use a more specialized logger, see https://github.com/camlworks/dream/issues/413 *)
  @@ Dream.logger
  @@ Dream.router (Api_maps.endpoints @ Api_heatmaps.endpoints @ Api_debug.endpoints store
                                      @ Web_static.endpoints @ Ws.endpoints) in
  signal := None;
  Dream.info (fun m -> m "Restarting web server");
  server ~error_handler store
