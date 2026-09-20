open Cmdliner
open Lwt.Syntax
open Zwm_fmt
open Zwm_io

let format_arg =
  let formats = [("csv", Csv); ("json", Json); ("toml", Toml); ("yaml", Yaml)] in
  let doc = "Format (" ^ (List.map fst formats |> String.concat "|") ^ ")" in
  Arg.(value & opt (enum formats) Yaml & info ["format"] ~docv:"FORMAT" ~doc)

let config_get fmt =
  let* v = get "config" in
  of_api fmt ~v Zwmlib.Types.config |> Lwt.return

let config_show fmt =
  let* config = config_get fmt in
  print_endline config;
  Lwt.return Cmd.Exit.ok

(** [0wm config show] subcommand *)
let zwm_config_show =
  let doc = "Show the 0WM daemon configuration" in
  let man = [
    `S Manpage.s_description;
    `P "Show the 0WM daemon configuration in the selected format.";
  ] in
  let info = Cmd.info "show" ~doc ~man in
  Term.(const config_show $ format_arg) |> Cmd.v info

let config_edit fmt =
  let editor = match Sys.getenv_opt "VISUAL" with
    | Some e -> e
    | None -> Sys.getenv_opt "EDITOR" |> Option.value ~default:"vi" in
  let* config = config_get fmt in
  let (filename, oc) = ext_of_fmt fmt |> Filename.open_temp_file "zwm_" in
  output_string oc config;
  close_out oc;
  let rec retry () =
    flush stderr;
    print_endline "Press Enter to open the editor again or Ctrl+C to abort";
    match read_line () with _ | exception _ -> edit ()
  and edit () = match Sys.command (editor ^ " " ^ Filename.quote filename) with
    | 0 -> begin
        let ic = open_in filename in
        let len = in_channel_length ic in
        let v = really_input_string ic len in
        close_in ic;
        try
          let json = to_api fmt ~v Zwmlib.Types.config in
          let* () = put "config" json in
          Lwt.return Cmd.Exit.ok
        with
        | Gendarme.Unknown_field f -> Printf.eprintf "Error: unknown field %s\n" f; retry ()
        | Gendarme.Type_error ->
            Printf.eprintf "Error: failed to validate configuration\n";
            retry ()
        | Server_error -> retry ()
        | _ -> Printf.eprintf "Error: an unknown error occurred\n"; retry ()
      end
    | code -> Printf.eprintf "Error: editor exited with code %i\n" code; retry () in
  let res = edit () in
  Sys.remove filename;
  res

(** [0wm config edit] subcommand *)
let zwm_config_edit =
  let doc = "Edit the application configuration" in
  let man = [
    `S Manpage.s_description;
    `P "Edit the application configuration using the configured editor.";
  ] in
  let info = Cmd.info "edit" ~doc ~man in
  Term.(const config_edit $ format_arg) |> Cmd.v info

let config_set l =
  let rec config_rec acc = function
    | ("interface", v)::tl -> config_rec Zwmapi.Types.{ acc with interface = Some v } tl
    | ("port", v)::tl -> begin match int_of_string_opt v with
        | None -> Error ("unknown integer " ^ v ^ " in port=" ^ v)
        | port -> config_rec { acc with port } tl
      end
    | ("aps", v)::tl -> config_rec { acc with aps = Some (String.split_on_char ',' v) } tl
    | ("ssids", v)::tl -> config_rec { acc with ssids = Some (String.split_on_char ',' v) } tl
    | (k, v)::_ -> Error ("unknown key " ^ k ^ " in " ^ k ^ "=" ^ v)
    | [] -> Ok acc in
  config_rec Zwmapi.Types.{ interface = None; port = None; aps = None; ssids = None } l
  |> Result.map (fun v ->
    let%lwt () = [%encode.Json] ~v Zwmapi.Types.config_patch |> patch "config" in
    Lwt.return Cmd.Exit.ok)

(** [0wm config set] subcommand *)
let zwm_config_set =
  let doc = "Modify the application configuration" in
  let man = [
    `S Manpage.s_description;
    `P "Modify the application configuration keys";
  ] in
  let info = Cmd.info "set" ~doc ~man in
  let kv = Arg.(pair ~sep:'=' string string) in
  let kv_arg = Arg.(non_empty & pos_all kv [] & info [] ~docv:"KEY=VALUE") in
  Term.(const config_set $ kv_arg) |> Term.cli_parse_result' |> Cmd.v info

(** [0wm config] subcommand *)
let cmd =
  let doc = "Configuration-related commands" in
  let man = [
    `S Manpage.s_description;
    `P "Inspect and modify the application configuration.";
  ] in
  let info = Cmd.info "config" ~doc ~man in
  let default = Term.(const (`Help (`Auto, Some "config")) |> ret) in
  Cmd.group info ~default [zwm_config_show; zwm_config_edit; zwm_config_set]
