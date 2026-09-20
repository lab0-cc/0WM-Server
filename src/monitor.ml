open Lwt.Syntax

let set_conf previous conf =
  let* () = Zwmlib.Store.set_conf conf Runtime.store in
  let () =
    if conf.interface <> previous.Zwmlib.Types.interface || conf.port <> previous.port
    then match !Web.signal with
      | Some resolver -> Lwt.wakeup_later resolver ()
      | None -> Dream.warning (fun m -> m "The web server is not running")
    else () in
  Lwt.return_unit

let server ~error_handler ~stop =
  let%lwt () = Dream.serve ~socket_path:Runtime.socket ~error_handler ~stop
    (* TODO: use a more specialized logger, see https://github.com/camlworks/dream/issues/413 *)
    @@ Dream.logger
    @@ Dream.router [
      Dream.get "/config" (fun _ ->
        let* v = Zwmlib.Store.get_conf Runtime.store in
        [%encode.Json] ~v Zwmlib.Types.config |> Util.json
      );
      Dream.patch "/config" (fun req ->
        let* current = Zwmlib.Store.get_conf Runtime.store in
        let* v = Dream.body req in
        let conf = [%decode.Json] ~v Zwmapi.Types.config_patch in
        let* () = set_conf current {
          interface = Option.value ~default:current.interface conf.interface;
          port = Option.value ~default:current.port conf.port;
          aps = Option.value ~default:current.aps conf.aps;
          ssids = Option.value ~default:current.ssids conf.ssids;
        } in
        Dream.empty `OK
      );
      Dream.put "/config" (fun req ->
        let* current = Zwmlib.Store.get_conf Runtime.store in
        let* v = Dream.body req in
        let conf = [%decode.Json] ~v Zwmlib.Types.config in
        let* () = set_conf current conf in
        Dream.empty `OK
      );
    ] in
  let () = match !Web.signal with
    | Some resolver ->
        Web.signal := None;
        Lwt.wakeup_later resolver ()
    | None -> () in
  Lwt.return_unit
