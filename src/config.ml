(** This module implements a rough configuration loader *)

[%%marshal.load Json]

type t = { interface: string [@json]; port: int [@json]; aps: string list [@json];
           ssids: string list [@json] } [@@marshal]

let config = [%unmarshal.Json] ~v:(Ezjsonm.from_channel (open_in "config.json")) t
