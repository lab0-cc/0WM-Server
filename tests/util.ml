(** This module provides helpers for tests *)

let f3 = Alcotest.float 1e-3
let f8 = Alcotest.float 1e-8

let rec rm_rf path = match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (function
        | "." | ".." -> ()
        | name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
  | _ ->
      Unix.unlink path

let temp_dir () = Filename.temp_dir "0wm-test-" ""
