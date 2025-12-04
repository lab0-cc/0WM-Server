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

type scan = { s_pos : Zwmlib.Linalg.point3; s_ts : int; s_meas : Zwmlib.Dot11_iwinfo.t list }
            [@@marshal]

type disp_one = { ssid : string option; signal : int; band : int }

type disp = { d_pos : Zwmlib.Linalg.point3; d_meas : disp_one list } [@@marshal]
