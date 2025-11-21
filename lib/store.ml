[%%marshal.load Ezjsonm]

open Lwt.Syntax
module Backend = Irmin_git_unix.FS.KV (Irmin.Contents.Json_value)
module Proj = Irmin.Json_tree (Backend)
module Info = Irmin_git_unix.Info (Backend.Info)

let src = Logs.Src.create "zwm.store" ~doc:"0WM storage backend"
module Log = (val Logs.src_log src : Logs.LOG)

type obj = {
  zmin : float [@json];
  zmax : float [@json];
  path : string [@json];
  anchors : Anchor.set [@json];
  height : float [@json];
  width : float [@json];
  structure : Geo.obj [@json] [@default Geo.Polygon []];
  walls : Linalg.Segment2.t list [@json];
  name : string [@json];
} [@@marshal]

type s_data = { position : Linalg.point3 [@json]; measurements : Dot11.measurement list [@json] }
              [@@marshal]

type s_meta = { map : string [@json]; transform : Linalg.Matrix3.t [@json] } [@@marshal]

type s_data_l = (int * s_data) list
let s_data_l = Gendarme.(map int s_data) (** Dirty trick until Gendarme supports more customization *)

type scan = { meta : s_meta option [@json]; data : s_data_l [@json] } [@@marshal]

type t = Backend.repo option ref

let (!!) s = match !s with
  | Some s -> s
  | None -> failwith "Store not initialized"

let get store path =
  let* s = Backend.main !!store in
  Backend.get s path

let set ~info store path contents =
  let* s = Backend.main !!store in
  Backend.set_exn ~info s path contents

let find_object id store =
  let* v = get store ["objects"; id] in
  [%unmarshal.Json] ~v obj |> Lwt.return

let find_object_j id store =
  let* v = get store ["objects"; id] in
  Ezjsonm.value_to_string v |> Lwt.return

let push_object id v store =
  Log.debug (fun m -> m "Adding object %s" id);
  let info = Info.v "Add object %s" id in
  [%marshal.Json] ~v obj |> set ~info store ["objects"; id]

let find_scan id store =
  let* branch = Backend.of_branch !!store ("scans/" ^ id) in
  let* v = Proj.get branch ["scans"; id] in
  [%unmarshal.Json] ~v scan |> Lwt.return

let begin_scan id store =
  Log.debug (fun m -> m "Beginning session %s" id);
  let store = !!store in
  let branch = "scans/" ^ id in
  match%lwt Backend.Branch.mem store branch with
  | true -> Backend.of_branch store branch
  | false ->
      let* main = Backend.main store in
      Backend.clone ~src:main ~dst:branch

let push_scan id ts v store =
  Log.debug (fun m -> m "Adding scan %i into %s" ts id);
  let info = Info.v "Add scan %i into %s" ts id in
  [%marshal.Json] ~v s_data |> Backend.set_exn ~info store ["scans"; id; "data"; string_of_int ts]

let set_meta id v store =
  Log.debug (fun m -> m "Setting metadata for %s" id);
  let info = Info.v "Set metadata for %s" id in
  [%marshal.Json] ~v s_meta |> Backend.set_exn ~info store ["scans"; id; "meta"]

let cleanup_scan ?(recovery=false) id store =
  let store = !!store in
  let branch_name = "scans/" ^ id in
  let* branch = Backend.of_branch store branch_name in
  match%lwt Backend.find_tree branch ["scans"; id] with
  | None ->
      Log.info (fun m -> m "Removing session %s" id);
      Backend.Branch.remove store branch_name
  | Some _ ->
      let* head = Backend.Head.get branch in
      let* main = Backend.main store in
      let* history = Backend.history main in
      if Backend.History.mem_vertex history head
      then Lwt.return_unit
      else
        let prefix = if recovery then "[recovery] " else "" in
        let info = Info.v "%sMerge session %s" prefix id in
        match%lwt Backend.merge_into ~into:main ~info branch with
        | Ok () ->
            Log.info (fun m -> m "Successfully merged session %s" id);
            Lwt.return_unit
        | Error (`Conflict s) ->
            Log.err (fun m -> m "Error merging session %s: %s" id s);
            Lwt.return_unit

let end_scan id =
  Log.debug (fun m -> m "Ending session %s" id);
  cleanup_scan id
