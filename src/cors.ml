let endpoint ?(methods=[]) path =
  (* TODO: these settings should be tuned *)
  let headers = [("Access-Control-Allow-Origin", "*"); ("Access-Control-Allow-Headers", "*")] in
  let headers = match methods with
    | [] -> headers
    | l -> ("Access-Control-Allow-Methods",
            List.map Dream.method_to_string l |> String.concat ",")::headers in
  Dream.options path (fun _ -> Dream.respond ~headers ~status:`No_Content "")
