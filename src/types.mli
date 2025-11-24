type map = { m_dst : float; m_cfd : Zwmlib.Confidence.t } [@@marshal]

type map_rec = { mr_dst : float; mr_cfd : Zwmlib.Confidence.t; mr_map : Zwmlib.Store.obj }
               [@@marshal]

type floorplan = { height : float; width : float; data : string }

type payload = {
  anchors : Zwmlib.Anchor.set;
  floorplan : floorplan;
  name : string;
  structure : Zwmlib.Linalg.point list list;
  walls : Zwmlib.Linalg.point list list;
  zmin : float;
  zmax : float;
} [@@marshal]
