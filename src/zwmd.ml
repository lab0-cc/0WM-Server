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
  Lwt_list.iter_s (fun (o, _) -> Api.push_to_rtree o) objects

(* This function is to be removed when https://github.com/camlworks/dream/pull/407 is merged
   upstream *)
let cleanup_socket () = match%lwt Lwt_unix.stat Runtime.socket with
  | exception Unix.Unix_error (ENOENT, _, _) -> Lwt.return_unit
  | { Unix.st_kind = S_SOCK; _ } ->
      Log.info (fun m -> m "Removing previous socket");
      Lwt_unix.unlink Runtime.socket
  | _ -> Lwt.return_unit

let rebuild_config store = match%lwt Zwmlib.Store.get_conf store with
  | exception _ ->
      Log.info (fun m -> m "Rebuilding configuration");
      Zwmlib.Store.set_conf (Gendarme.default Zwmlib.Types.config ()) store
  | _ -> Lwt.return_unit

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

let init =
  Dream.initialize_log ();
  let* store = Runtime.(Store.Repo.v config) in
  Runtime.store := Some store;
  let* branches = Runtime.Store.Branch.list store >|= List.filter (String.starts_with ~prefix:"scans/") in
  let* () = cleanup_open_sessions Runtime.store branches in
  let* () = rebuild_rtree store in
  let* () = cleanup_socket () in
  let* () = rebuild_config Runtime.store in
  Log.info (fun m -> m "Initialization completed");
  Lwt.join [Web.server ~error_handler store; Monitor.server ~error_handler]

let () = Lwt_main.run init
