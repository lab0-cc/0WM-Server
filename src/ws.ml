open Zwmlib
open Lwt.Syntax

let (>>=) = Option.bind

type context = { branch: Store.Backend.t option; uuid: string option }
let empty_context = { branch = None; uuid = None }

let uuid_opt =
  let now_ms () = Unix.gettimeofday () *. 1000. |> Int64.of_float in
  Random.State.make_self_init () |> Uuidm.v7_monotonic_gen ~now_ms

let rec uuid () = match uuid_opt () with
  | Some uuid -> Uuidm.to_string uuid
  | None -> Unix.sleepf 0.001; uuid ()

let init ?id _ =
  let v = match [%decode.Json] ?v:id Gendarme.(option string) >>= Uuidm.of_string with
    | Some id -> Uuidm.to_string id
    | None -> uuid () in
  let* b = Store.begin_scan v Runtime.store in
  Lwt.return ("UUID\000" ^ [%encode.Json] ~v Gendarme.string,  { branch = Some b; uuid = Some v })

let scan v = function
  | { branch = None; _ } | { uuid = None; _ } -> failwith "Not initialized"
  | { branch = Some branch; uuid = Some uuid } as context ->
      let open Commands in
      let { s_pos; s_ts; s_meas } = [%decode.Json] ~v scan in
      let measurements = List.map Dot11_iwinfo.measurement s_meas in
      let* () = Store.push_scan uuid s_ts { position = s_pos; measurements} branch in
      let top = match List.sort (fun Dot11.{ signal = s; _ } { signal = s'; _ } -> compare s' s)
                                measurements with
        | m::m'::m''::_ -> [m; m'; m'']
        | l -> l in
      let d_meas = List.map (fun Dot11.{ ap = { ssid; channel; _ }; signal } ->
                               { ssid; signal; band = Dot11.band_of_channel channel }) top in
      let v = { d_pos = s_pos; d_meas } in
      Lwt.return ("DISP\000" ^ [%encode.Json] ~v disp, context)

let meta v = function
  | { branch = None; _ } | { uuid = None; _ } -> failwith "Not initialized"
  | { branch = Some branch; uuid = Some uuid } as context ->
      let metadata = [%decode.Json] ~v Store.s_meta in
      let* () = Store.set_meta uuid metadata branch in
      Lwt.return ("", context)

let noap context =
  Lwt.return ("TRYL\000" ^ [%encode.Json] ~v:Config.config.aps Gendarme.(list string), context)

let rqht ({ uuid; _ } as context) = match uuid with
  | None -> failwith "No UUID"
  | Some uuid ->
      let* v = Api.get_heatmap_s ~ssids:Config.config.ssids uuid in
      Lwt.return ("HEAT\000" ^ [%encode.Json] ~v Gendarme.(pair string (Linalg.Box2.t)), context)

let rec live ?(context=empty_context) ws =
  match%lwt Dream.receive ws with
  | Some s -> begin
      let command = match String.split_on_char '\000' s with
        | "INIT"::[] -> init ?id:None
        | "INIT"::id::[] -> init ~id
        | "META"::metadata::[] -> meta metadata
        | "NOAP"::[] -> noap
        | "RQHT"::[] -> rqht
        | "SCAN"::data::[] -> scan data
        | _ -> failwith "Unknown command" in
      let* (payload, context) = command context in
      let promise = match payload with
        | "" -> Lwt.return_unit
        | _ -> Dream.send ws payload in
      let%lwt () = promise in
      live ~context ws
    end
  | _ ->
      let* () = match context with
        | { uuid = Some uuid; _ } -> Store.end_scan uuid Runtime.store
        | _ -> Lwt.return_unit in
      Dream.close_websocket ws
