open Lwt.Syntax
open Lwt.Infix

let src = Logs.Src.create "zwm.init" ~doc:"0WM initialization routine"
module Log = (val Logs.src_log src : Logs.LOG)

let cleanup_open_sessions store =
  Log.info (fun m -> m "Cleaning up open sessions");
  Lwt_list.iter_s (fun b -> Zwmlib.Store.cleanup_scan (String.sub b 6 36) store)

let rebuild_rtree store =
  Log.info (fun m -> m "Rebuilding R-tree");
  let* main = Runtime.Store.main store in
  let* objects = Runtime.Store.list main ["objects"] in
  Lwt_list.iter_s (fun (o, _) -> Runtime.rtree_push o) objects

let init_data_dir () =
  let path = Runtime.var "data" in
  match%lwt Lwt_unix.stat path with
    | { Unix.st_kind = S_DIR; _ } -> Lwt.return_unit
    | _ -> invalid_arg "The data path is not a directory"
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        Log.info (fun m -> m "Creating data directory");
        Lwt_unix.mkdir path 0o750

let rebuild_config store =
  let%lwt current = match%lwt Zwmlib.Store.get_conf store with
    | exception _ ->
        Log.info (fun m -> m "Rebuilding configuration");
        let config = Gendarme.default Zwmlib.Types.config () in
        let%lwt () = Zwmlib.Store.set_conf config store in
        Lwt.return config
    | config -> Lwt.return config in
  let spool = match Sys.getenv_opt "ZWM_SPOOL" with
    | Some dir -> dir
    | None -> "/var/spool/0wm" in
  let config_file = Filename.concat spool "config" in
  try
    let ic = open_in config_file in
    let len = in_channel_length ic in
    let v = really_input_string ic len in
    close_in ic;
    Sys.remove config_file;
    let conf = [%decode.Json] ~v Zwmapi.Types.config_patch in
    Zwmlib.Store.set_conf {
      interface = Option.value ~default:current.interface conf.interface;
      port = Option.value ~default:current.port conf.port;
      aps = Option.value ~default:current.aps conf.aps;
      ssids = Option.value ~default:current.ssids conf.ssids;
    } store
  with _ -> Lwt.return_unit

let notify_systemd () = match Sys.getenv_opt "NOTIFY_SOCKET" with
  | None -> ()
  | Some sock ->
      let sock = match sock.[0] with
        | '@' -> "\000" ^ String.sub sock 1 (String.length sock - 1)
        | _ -> sock in
      let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
      try
        Unix.connect fd (Unix.ADDR_UNIX sock);
        let n = Unix.write_substring fd "READY=1" 0 7 in
        Unix.close fd;
        if n <> 7 then failwith "Truncated message"
      with exn -> Log.warn (fun m -> Printexc.to_string exn
                                     |> m "Failed signaling readiness to systemd: %s")

let error_handler (Dream.{ condition; will_send_response; _ } as e) = match condition with
  | `Exn (Util.Bad_parameter parameter) when will_send_response ->
      Dream.respond ~status:`Bad_Request ("Bad parameter: " ^ parameter) >|= Option.some
  | `Exn (Ezjsonm.Parse_error (_, s)) when will_send_response ->
      Dream.respond ~status:`Bad_Request ("JSON parse error: " ^ s) >|= Option.some
  | `Exn (Gendarme.Unknown_field field) when will_send_response ->
      Dream.respond ~status:`Bad_Request ("Unknown field: " ^ field) >|= Option.some
  | `Exn Gendarme.Type_error when will_send_response ->
      Dream.respond ~status:`Bad_Request "Request type error" >|= Option.some
  | `Exn Zwmlib.Image.Unrecognized_format when will_send_response ->
      Dream.respond ~status:`Bad_Request "Unrecognized image format" >|= Option.some
  | `Exn (Invalid_argument _) when will_send_response ->
      Dream.respond ~status:`Not_Found "Not found" >|= Option.some
  | _ -> Dream.debug_error_handler e

let stop, resolver = Lwt.wait ()
let quit _ = Lwt.wakeup_later resolver ()

let init =
  Dream.initialize_log ();
  let* store = Runtime.(Store.Repo.v config) in
  Runtime.store := Some store;
  let* branches = Runtime.Store.Branch.list store >|= List.filter (String.starts_with ~prefix:"scans/") in
  let* () = cleanup_open_sessions Runtime.store branches in
  let* () = rebuild_rtree store in
  let* () = init_data_dir () in
  let* () = rebuild_config Runtime.store in
  notify_systemd ();
  Log.info (fun m -> m "Initialization completed");
  if Sys.unix
  then begin
    Lwt_unix.on_signal Sys.sigint quit |> ignore;
    Lwt_unix.on_signal Sys.sigterm quit |> ignore
  end;
  Lwt.join [Web.server ~error_handler store; Monitor.server ~error_handler ~stop]

let () =
  if Sys.unix then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Lwt_main.run init
