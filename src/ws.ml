open Zwmlib

let (>>=) = Option.bind

type context = { uuid: string option }
let empty_context = { uuid = None }

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
  ("UUID\000" ^ [%encode.Json] ~v Gendarme.string,  { uuid = Some v })

let scan data ({ uuid } as context) = match uuid with
  | None -> failwith "No UUID"
  | Some uuid ->
      let open Commands in
      let { s_pos; s_ts; s_meas } = [%decode.Json] ~v:data scan in
      let measurements = List.map Dot11_iwinfo.measurement s_meas in
      Scan_store.push uuid { position = s_pos; timestamp = s_ts; measurements} Storage.measurements;
      let top = match List.sort (fun Dot11.{ signal = s; _ } { signal = s'; _ } -> compare s' s)
                                measurements with
        | m::m'::m''::_ -> [m; m'; m'']
        | l -> l in
      let d_meas = List.map (fun Dot11.{ ap = { ssid; channel; _ }; signal } ->
                               { ssid; signal; band = Dot11.band_of_channel channel }) top in
      let v = { d_pos = s_pos; d_meas } in
      ("DISP\000" ^ [%encode.Json] ~v disp, context)

let noap context =
  ("TRYL\000" ^ [%encode.Json] ~v:Config.config.aps Gendarme.(list string),
   context)

let rqht ({ uuid } as context) = match uuid with
  | None -> failwith "No UUID"
  | Some uuid ->
      ("HEAT\000" ^ [%encode.Json] ~v:(Api.get_heatmap_s ~ssids:Config.config.ssids uuid)
                                   Gendarme.(pair string (Linalg.Box2.t)), context)

let rec live ?(context=empty_context) ws =
  match%lwt Dream.receive ws with
  | Some s -> begin
      let command = match String.split_on_char '\000' s with
        | "INIT"::[] -> init ?id:None
        | "INIT"::id::[] -> init ~id
        | "NOAP"::[] -> noap
        | "RQHT"::[] -> rqht
        | "SCAN"::data::[] -> scan data
        | _ -> failwith "Unknown command" in
      let (payload, context) = command context in
      let%lwt () = Dream.send ws payload in
      live ~context ws
    end
  | _ -> Dream.close_websocket ws
