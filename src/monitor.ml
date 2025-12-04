open Lwt.Syntax

let server ~error_handler =
  Dream.serve ~socket_path:Runtime.socket ~error_handler
  (* TODO: use a more specialized logger, see https://github.com/camlworks/dream/issues/413 *)
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/config" (fun _ ->
      let* v = Zwmlib.Store.get_conf Runtime.store in
      [%encode.Json] ~v Zwmlib.Types.config |> Util.json
    );
    Dream.post "/config" (fun req ->
      let* config = Zwmlib.Store.get_conf Runtime.store in
      let* v = Dream.body req in
      let conf = [%decode.Json] ~v Zwmlib.Types.config in
      let* () = Zwmlib.Store.set_conf conf Runtime.store in
      let () =
        if config.interface <> conf.interface || config.port <> conf.port
        then match !Web.signal with
          | Some resolver -> Lwt.wakeup_later resolver ()
          | None -> Dream.warning (fun m -> m "The web server is not running") in
      Dream.empty `OK
    );
  ]
