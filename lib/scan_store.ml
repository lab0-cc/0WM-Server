[%%marshal.load Json]

type scan = { position: Linalg.point3 [@json]; timestamp: int [@json];
              measurements: Dot11.measurement list [@json] } [@@marshal]

type metadata = { map: string [@json]; transform: Linalg.Matrix3.t [@json] } [@@marshal]

type t = (string, metadata option * scan list) Hashtbl.t [@@marshal]

let empty = Hashtbl.create 16

let push id scan store = match Hashtbl.find_opt store id with
  | Some (meta, scans) -> Hashtbl.replace store id (meta, scan::scans)
  | None -> Hashtbl.add store id (None, [scan])

let find id store = match Hashtbl.find_opt store id with
  | Some s -> s
  | None -> (None, [])

let meta id meta store = match Hashtbl.find_opt store id with
  | Some (_, scans) -> Hashtbl.replace store id (Some meta, scans)
  | None -> Hashtbl.add store id (Some meta, [])
