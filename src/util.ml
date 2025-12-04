exception Bad_parameter of string

let csv data = Dream.respond ~headers:[("Content-Type", "text/csv");
                                       ("Access-Control-Allow-Origin", "*")] data
let json data = Dream.json ~headers:[("Access-Control-Allow-Origin", "*")] data
let toml data = Dream.respond ~headers:[("Content-Type", "application/toml");
                                        ("Access-Control-Allow-Origin", "*")] data
let yaml data = Dream.respond ~headers:[("Content-Type", "application/yaml");
                                        ("Access-Control-Allow-Origin", "*")] data
let html data = Dream.html ~headers:[("Access-Control-Allow-Origin", "*")] data
let svg data = Dream.respond ~headers:[("Content-Type", "image/svg+xml");
                                       ("Access-Control-Allow-Origin", "*")] data

let ok () = Dream.empty ~headers:[("Content-Type", "image/svg+xml");
                                  ("Access-Control-Allow-Origin", "*")] `OK

