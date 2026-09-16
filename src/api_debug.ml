open Lwt.Syntax

let debug_store ?branch ~store path =
  let* branch = match branch with
    | None | Some "main" -> Runtime.Store.main store
    | Some b -> Runtime.Store.of_branch store b in
  let* data = Runtime.Proj.get branch path in
  Ezjsonm.value_to_string data |> Dream.json

let endpoints store = [
    Dream.get "/debug/rtree" (fun _ -> [%encode.Json] ~v:!Runtime.rtree (Gendarme.option Zwmlib.Rtree.t) |> Dream.json);
    Dream.get "/debug/store" (fun request -> debug_store ?branch:(Dream.query request "branch") ~store []);
    Dream.get "/debug/store/**" (fun request ->
      (Dream.path [@alert "-deprecated"]) request |> List.filter ((<>) "")
      |> debug_store ?branch:(Dream.query request "branch") ~store);
]
