let eps = 1e-9
let c10 = 10. /. log 10.

let par_map ~domains ~zero f arr =
  let pool = Domainslib.Task.setup_pool ~num_domains:(domains - 1) () in
  let n = Array.length arr in
  let res = Array.make n zero in
  Fun.protect ~finally:(fun () -> Domainslib.Task.teardown_pool pool) @@ fun () ->
    Domainslib.Task.run pool (fun () ->
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(n - 1)
                                        ~body:(fun i -> res.(i) <- f arr.(i)));
  res

let par_init ~domains ~zero n f =
  let pool = Domainslib.Task.setup_pool ~num_domains:(domains - 1) () in
  let res = Array.make n zero in
  Fun.protect ~finally:(fun () -> Domainslib.Task.teardown_pool pool) @@ fun () ->
    Domainslib.Task.run pool (fun () ->
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(n - 1)
                                        ~body:(fun i -> res.(i) <- f i));
  res

let cat = String.concat "\n"

let fold_range f init start stop =
  let rec loop acc i =
    if i > stop
    then acc
    else loop (f acc i) (i + 1) in
  loop init start

let quickselect i =
  if i < 0 then invalid_arg "quickselect";
  let partition_counts pivot xs =
    let rec loop l eq r lc ec rc = function
      | [] -> (l, eq, r, lc, ec, rc)
      | hd::tl -> match compare hd pivot with
          | c when c < 0 -> loop (hd::l) eq r (lc + 1) ec rc tl
          | 0 -> loop l (hd::eq) r lc (ec + 1) rc tl
          | _ -> loop l eq (hd::r) lc ec (rc + 1) tl in
    loop [] [] [] 0 0 0 xs in
  let rec select i = function
    | [] -> invalid_arg "quickselect"
    | hd::tl -> match partition_counts hd tl with
        | (l, _, _, lc, _, _) when i < lc -> select i l
        | (_, _, _, lc, ec, _) when i < lc + ec + 1 -> hd
        | (_, _, r, lc, ec, _) -> select (i - lc - ec - 1) r in
  select i
