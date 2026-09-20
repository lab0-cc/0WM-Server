open Cmdliner

(** CLI entry point *)
let () =
  let doc = "0WM command-line client" in
  let man = [
    `S Manpage.s_description;
    `P "This tool allows to interact locally with the 0WM daemon.";
  ] in
  let info = Cmd.info "0wm" ~version:"0.1" ~doc ~man in
  let default = Term.(const (`Help (`Auto, None)) |> ret) in
  match Cmd.group info ~default [Zwm_config.cmd] |> Cmd.eval_value with
  | Ok (`Ok action) -> begin match Lwt_main.run action with
      | _ -> exit 0
      | exception _ -> exit 123
    end
  | Ok _ -> exit 0
  | Error _ -> exit 1
