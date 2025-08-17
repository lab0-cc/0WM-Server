(** This module implements a rough configuration loader *)

[%%marshal.load Yojson]

type t = { interface: string [@json]; port: int [@json]; aps: string list [@json] } [@@marshal]

let config = [%unmarshal.Json] ~v:(Yojson.Safe.from_file "config.json") t
