(** This module provides the types used throughout the API *)

[%%marshal.load Json]

open Zwmlib.Linalg

type map = {
  m_dst : float [@json "distance"];
  m_cfd : Zwmlib.Confidence.t [@json "confidence"] [@default Invalid];
} [@@marshal]

type map_rec = {
  mr_dst : float [@json "distance"];
  mr_cfd : Zwmlib.Confidence.t [@json "confidence"] [@default Invalid];
  mr_map : Zwmlib.Store.obj [@json "map"];
} [@@marshal]

type floorplan = { height : float [@json]; width : float [@json]; data : string [@json] }
                 [@@marshal]

type payload = {
  anchors : Zwmlib.Anchor.set [@json];
  floorplan : floorplan [@json];
  name : string [@json];
  structure : point list list [@json];
  walls : point list list [@json];
  zmin : float [@json];
  zmax : float [@json];
} [@@marshal]

type scan = {
  s_pos : point3 [@json "position"];
  s_ts : int [@json "timestamp"];
  s_meas : Zwmlib.Dot11_iwinfo.t list [@json "measurements"];
} [@@marshal]

type disp_one = { ssid : string option [@json]; signal : int [@json]; band : int [@json] }
                [@@marshal]

type disp = {
  d_pos : point3 [@json "position"];
  d_meas : disp_one list [@json "measurements"];
} [@@marshal]
