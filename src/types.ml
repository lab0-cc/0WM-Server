(** This module provides the types used throughout the API *)

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

type floorplan = { height : float; width : float; data : string } [@@marshal { tag = json }]

type payload = {
  anchors : Zwmlib.Anchor.set;
  floorplan : floorplan;
  name : string;
  structure : point list list;
  walls : point list list;
  zmin : float;
  zmax : float;
} [@@marshal { tag = json }]

type scan = {
  s_pos : point3 [@json "position"];
  s_ts : int [@json "timestamp"];
  s_meas : Zwmlib.Dot11_iwinfo.t list [@json "measurements"];
} [@@marshal]

type disp_one = { ssid : string option; signal : int; band : int } [@@marshal { tag = json }]

type disp = {
  d_pos : point3 [@json "position"];
  d_meas : disp_one list [@json "measurements"];
} [@@marshal]

type config_patch = { interface : string option; port : int option; aps : string list option;
                      ssids : string list option } [@@marshal { omit_default; tag = json }]
