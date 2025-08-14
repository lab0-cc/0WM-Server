[%%marshal.load Json]

open Zwmlib

type scan = { s_pos: Linalg.point3 [@json "position"]; s_ts: int [@json "timestamp"];
              s_meas: Dot11_iwinfo.t list [@json "measurements"] } [@@marshal]

type disp_one = { ssid: string option [@json]; signal: int [@json]; band: int [@json] } [@@marshal]

type disp = { d_pos: Linalg.point3 [@json "position"];
              d_meas: disp_one list [@json "measurements"] } [@@marshal]
