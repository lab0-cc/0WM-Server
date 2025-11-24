type scan = { s_pos : Zwmlib.Linalg.point3; s_ts : int; s_meas : Zwmlib.Dot11_iwinfo.t list }
            [@@marshal]
type disp_one = { ssid : string option; signal : int; band : int }
type disp = { d_pos : Zwmlib.Linalg.point3; d_meas : disp_one list } [@@marshal]
