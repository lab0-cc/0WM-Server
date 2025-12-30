(** Run the test suite *)
let () =
  Lwt_main.run @@ Alcotest_lwt.run "Zwmlib" [
    ("anchor", Lanchor.suite);
    ("color", Lcolor.suite);
    (* Confidence is not checked *)
    ("dot11", Ldot11.suite);
    ("dot11_iwinfo", Ldot11_iwinfo.suite);
    ("geo", Lgeo.suite);
    ("heap", Lheap.suite);
    (* Image is not checked *)
    ("linalg", Llinalg.suite);
    ("math", Lmath.suite);
    (* Png is not checked *)
    ("power", Lpower.suite);
    (* Render is not checked *)
    ("rtree", Lrtree.suite);
    (* Sim is not checked *)
    (* Store is not checked *)
    (* Svg is not checked *)
    (* Types is not checked *)
    ("util", Lutil.suite);
  ]
