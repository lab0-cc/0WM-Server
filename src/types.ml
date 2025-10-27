[%%marshal.load Json]

open Zwmlib.Linalg

type map = { m_dst: float [@json "distance"];
             m_cfd: Zwmlib.Confidence.t [@json "confidence"] [@default Invalid] } [@@marshal]

type map_rec = { mr_dst: float [@json "distance"];
                 mr_cfd: Zwmlib.Confidence.t [@json "confidence"] [@default Invalid];
                 mr_map: Zwmlib.Object_store.obj [@json "map"] } [@@marshal]

type floorplan = { height: float [@json];
                   width: float [@json];
                   data: string [@json] } [@@marshal]

type payload = { anchors: Zwmlib.Anchor.set [@json];
                 floorplan: floorplan [@json];
                 name: string [@json];
                 structure: point list list [@json];
                 walls: point list list [@json];
                 zmin: float [@json];
                 zmax: float [@json] } [@@marshal]
