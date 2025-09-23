[%%marshal.load Json]

open Zwmlib.Linalg

type map = { m_dst: float [@json "distance"];
             m_cfd: Zwmlib.Confidence.t [@json "confidence"] [@default Invalid];
             m_id: string [@json "id"] } [@@marshal]

type floorplan = { height: float [@json];
                   width: float [@json];
                   data: string [@json] } [@@marshal]

type payload = { anchors: Zwmlib.Anchor.set [@json];
                 floorplan: floorplan [@json];
                 name: string [@json];
                 shapes: point list list [@json];
                 zmin: float [@json];
                 zmax: float [@json] } [@@marshal]
