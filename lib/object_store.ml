[%%marshal.load Json]

type obj = { zmin: float [@json]; zmax: float [@json]; path: string [@json];
             anchors: Anchor.set [@json]; height: float [@json]; width: float [@json];
             structure: Geo.obj [@json] [@default Geo.Polygon []];
             walls: Linalg.Segment2.t list [@json]; name: string [@json] } [@@marshal]
type t = (string, obj) Hashtbl.t [@@marshal]

let find obj store = Hashtbl.find store obj

let empty = Hashtbl.create 16

let push id obj store = Hashtbl.add store id obj
