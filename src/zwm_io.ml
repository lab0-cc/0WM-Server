open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Lwt.Infix

exception Server_error

let socket = Sys.getenv_opt "ZWM_SOCKET" |> Option.value ~default:"zwmd.sock"

let ctx =
  let h = Hashtbl.create 1 in
  Hashtbl.add h "zwmd" (`Unix_domain_socket socket);
  Client.custom_ctx ~resolver:(Resolver_lwt_unix.static h) ()

let get endpoint =
  let uri = Uri.of_string ("http://zwmd/" ^ endpoint) in
  Client.get ~ctx uri >>= fun (resp, body) ->
    match Response.status resp with
    | #Code.success_status -> Body.to_string body
    | code ->
        Code.string_of_status code |> Printf.eprintf "Error: server returned %s\n";
        raise Server_error

let post endpoint json =
  let uri = Uri.of_string ("http://zwmd/" ^ endpoint) in
  let headers = Header.init () |> fun h -> Header.add h "Content-Type" "application/json" in
  let body = Body.of_string json in
  Client.post ~ctx ~headers ~body uri >>= fun (resp, _) ->
    let () = match Response.status resp with
      | #Code.success_status -> ()
      | code ->
          Code.string_of_status code |> Printf.eprintf "Error: server returned %s\n";
          raise Server_error in
    Lwt.return_unit
