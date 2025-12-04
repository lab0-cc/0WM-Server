[%%marshal.load Csv (Json)]

type fmt = Csv | Json | Toml | Yaml

let ext_of_fmt = function
  | Csv -> ".csv"
  | Json -> ".json"
  | Toml -> ".toml"
  | Yaml -> ".yaml"

let of_api fmt ~v t =
  let encode = match fmt with
    | Csv -> [%encode.Csv.Json]
    | Json -> [%encode.Json]
    | Toml -> [%encode.Toml]
    | Yaml -> [%encode.Yaml] in
  encode ~v:([%decode.Json] ~v t) t

let to_api fmt ~v t =
  let decode = match fmt with
    | Csv -> [%decode.Csv.Json]
    | Json -> [%decode.Json]
    | Toml -> [%decode.Toml]
    | Yaml -> [%decode.Yaml] in
  [%encode.Json] ~v:(decode ~v t) t
