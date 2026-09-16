module U = Util
open Lwt.Syntax
open Zwmlib
open Linalg
open Types

let get_heatmap_s ~ssids id =
  let t = Hashtbl.create 10 in
  let* { meta; data } = Store.find_scan id Runtime.store in
  List.iter (fun (_, Store.{ position = { p3_x; p3_z; _ }; measurements; _ }) ->
    List.iter (fun Dot11.{ ap; signal } -> match ap.ssid with
      | Some ssid when List.mem ssid ssids ->
          let f = Dot11.center_freq ap in
          let l = Hashtbl.find_opt t f |> Option.value ~default:[] in
          Hashtbl.replace t (Dot11.center_freq ap)
                          ({ s_p = { p_x = p3_x; p_y = p3_z }; s_dbm = float signal }::l)
      | _ -> ()
    ) measurements) data;
  let spectrum =
    Hashtbl.fold (fun f s_samples acc -> { s_freq = float f; s_samples; s_aps = [] }::acc) t [] in
  let* walls = match meta with
    | Some meta ->
        let* Store.{ structure; walls; _ } = Store.find_object meta.map Runtime.store in
        Geo.segments structure @ walls
        |> List.map (fun s -> { w_seg = Matrix3.apply_s2 s meta.transform; w_tran = 0.6;
                                w_refl = 0.3 })
        |> Lwt.return
    | None -> Lwt.return [] in
  let env = { walls; spectrum } in
  let render_cfg = Render.{ resolution = 0.2; padding = 5.; scale = 40.0; steps = 0; vmin = -90.;
                            vmax = -30.; minimal = true } in
  let sim_cfg = Sim.{ exponent = 2.0; precision = 0.25; norm = 10.; blend_sharpness = 2. } in
  let f = Sim.build_predictor ~cfg:sim_cfg env in
  Render.render ~cfg:render_cfg ~f env |> Lwt.return

(** Get a SVG heatmap by its ID *)
let get ~ssids id = let* (img, _) = get_heatmap_s ~ssids id in U.svg img

let endpoints = [
  Dream.get "/heatmaps/:id" (fun request ->
    Dream.param request "id" |> get ~ssids:(Dream.queries request "ssid")
  )
]
