[%%marshal.load Csv; Csv (Json); Json; Toml; Yaml]

type wall = { w_seg : Linalg.Segment2.t; w_tran : float; w_refl : float }
type sample = { s_p : Linalg.point; s_dbm : float }
type ap = { a_p : Linalg.point; a_dbm : float option }
type spectrum_datum = { s_freq : float; s_samples : sample list; s_aps : ap list }
type env = { walls : wall list; spectrum : spectrum_datum list }

type config = {
  interface : string [@csv.json] [@json] [@toml] [@yaml] [@default "localhost"];
  port : int [@csv.json] [@json] [@toml] [@yaml] [@default 8080];
  aps : string list [@csv.json] [@json] [@toml] [@yaml];
  ssids : string list [@csv.json] [@json] [@toml] [@yaml];
} [@@marshal disallow_unknown_fields]
