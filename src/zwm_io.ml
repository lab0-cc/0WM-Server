open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

exception Server_error
exception Client_error

let has_zwm_socket dir =
  let path = Filename.concat dir "0wmd.sock" in
  match (Unix.stat path).st_kind with
  | S_SOCK -> Some path
  | _ | exception _ -> None

let ctx () =
  let socket = match Sys.getenv_opt "ZWM_RUN" with
    | Some dir -> Filename.concat dir "0wmd.sock"
    | None -> match List.find_map has_zwm_socket ["."; "/run/0wm"; "/var/run/0wm"] with
        | Some dir -> dir
        | None ->
            Printf.eprintf "0WM daemon socket not found\n";
            raise Client_error in
  let h = Hashtbl.create 1 in
  Hashtbl.add h "zwmd" (`Unix_domain_socket socket);
  Client.custom_ctx ~resolver:(Resolver_lwt_unix.static h) ()

let err _ =
  Printf.eprintf "Error connecting to the socket. Is the daemon running and are you allowed to \
                  access it?\n";
  raise Client_error

let get endpoint =
  let uri = Uri.of_string ("http://zwmd/" ^ endpoint) in
  let ctx = ctx () in
  Lwt.try_bind
    (fun () -> Client.get ~ctx uri)
    (fun (resp, body) -> match Response.status resp with
     | #Code.success_status -> Body.to_string body
     | code ->
         Code.string_of_status code |> Printf.eprintf "Error: server returned %s\n";
         raise Server_error)
    err

let post endpoint json =
  let uri = Uri.of_string ("http://zwmd/" ^ endpoint) in
  let ctx = ctx () in
  let headers = Header.init () |> fun h -> Header.add h "Content-Type" "application/json" in
  let body = Body.of_string json in
  Lwt.try_bind
    (fun () -> Client.post ~ctx ~headers ~body uri)
    (fun (resp, _) -> match Response.status resp with
     | #Code.success_status -> Lwt.return_unit
     | code ->
         Code.string_of_status code |> Printf.eprintf "Error: server returned %s\n";
         raise Server_error)
    err
