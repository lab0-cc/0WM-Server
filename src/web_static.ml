let endpoints = [
    Dream.get "/data/**" (Runtime.var "data" |> Dream.static);
    Dream.get "/swagger/**" (Runtime.static "swagger" |> Dream.static);
    Dream.get "/" (Dream.from_filesystem (Runtime.static "static") "api.html");
    (* TODO: generate this file *)
    Dream.get "/api.yml" (Dream.from_filesystem (Runtime.static "static") "api.yml");
]
