module U = Util

let get_parameter f request parameter =
  Dream.query request parameter |> Option.map (fun v -> match f v with
    | Some v -> v
    | None -> raise (U.Bad_parameter parameter))

let get_float = get_parameter Float.of_string_opt

let get_int = get_parameter int_of_string_opt

let get_toggle request parameter = match Dream.query request parameter with
  | None -> false
  | Some s -> match String.lowercase_ascii s with
      | "false" | "no" | "disable" | "0" -> false
      | _ -> true
