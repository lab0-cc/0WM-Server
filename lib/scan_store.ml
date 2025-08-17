[%%marshal.load Json]

type scan = { position: Linalg.point3 [@json]; timestamp: int [@json];
              measurements: Dot11.measurement list [@json] } [@@marshal]

type t = (string, scan) Hashtbl.t [@@marshal]

let empty = Hashtbl.create 16

let push id scan store = Hashtbl.add store id scan
