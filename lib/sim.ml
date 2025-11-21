open Linalg
open Types

type config = { exponent : float; precision : float; norm : float; blend_sharpness : float }

type model = {
  calibration : Linalg.Vector.t;
  pts : Linalg.point array;
  cfg : config;
  walls : Types.wall list;
  aps : Types.ap list;
  kernel : Linalg.Matrix.t;
  ell : float;
  freq : float;
}

(** Direct radio distance between two points *)
let radio_dist ~ell ~walls p p' =
  let base = Point2.dist p p' in
  let seg = Segment2.of_points p p' in
  List.fold_left
    (fun acc w ->
       if Segment2.crosses seg w.w_seg
       then acc -. ell *. 1.329 *. log10 w.w_tran
       else acc)
    base walls

(** Transmission product along a segment *)
let tx ?skip ~walls p p' =
  List.fold_left
    (fun acc w -> match skip with
       | Some w' when w == w' -> acc
       | _ -> if Segment2.(of_points p p' |> crosses w.w_seg) then acc *. w.w_tran else acc)
    1. walls

(** Single-bounce reflection *)
let reflection_tx ~walls w ap p =
  match Segment2.(mirror_p ap w.w_seg |> of_points p |> intersect w.w_seg) with
  | Inside p' -> Some (tx ~skip:w ~walls ap p' *. w.w_refl *. tx ~skip:w ~walls p' p,
                       Point2.(dist ap p' +. dist p' p))
  | _ -> None

(** AP path gain *)
let ap_path_gain ~exponent ~freq ~walls ap p =
  let gain = (299_792_458.0 /. (4.0 *. Float.pi *. freq)) ** 2. in
  let rx r = gain /. (max r 1.) ** exponent in
  let p_tx = match ap.a_dbm with Some v -> Power.mW v | None -> 1.0 in
  let direct = p_tx *. tx ~walls ap.a_p p *. rx (Point2.dist ap.a_p p) in
  let refl = List.fold_left
               (fun acc w -> match reflection_tx ~walls w ap.a_p p with
                  | Some (mult, l) -> acc +. p_tx *. mult *. rx l
                  | None -> acc)
               0. walls in
  direct +. refl

(** Prior power at a given point *)
let prior_dbm ~exponent ~freq ~walls ~aps ~norm p =
  (List.fold_left (fun acc ap -> acc +. (ap_path_gain ~exponent ~freq ~walls ap p) ** norm)
                  0.0 aps |> Power.dBm) /. norm |> max (-120.)

(** Evaluate the model at a generic point *)
let predict m p =
  let prior = prior_dbm ~exponent:m.cfg.exponent ~freq:m.freq ~walls:m.walls ~aps:m.aps
                        ~norm:m.cfg.norm p in
  let cov = Array.map (fun p' ->
    Math.matern32 (radio_dist ~ell:m.ell ~walls:m.walls p p' /. m.ell)) m.pts in
  let confidence = cg_solve m.kernel cov |> Vector.dot cov in
  (prior +. (if m.aps = [] then 1. else confidence) *. Vector.dot cov m.calibration, confidence)

(** Precompute prior AP gains at sample points and keep track of unknown AP powers *)
let precompute ~cfg ~freq ~walls ~aps samples =
  let rec precompute_rec (pow, unknown) = function
    | [] -> (pow, List.rev unknown |> Matrix.of_vectors)
    | hd::tl ->
        let ap_gains = Array.map (fun { s_p; _ } ->
          ap_path_gain ~exponent:cfg.exponent ~freq ~walls hd s_p) samples in
        match hd.a_dbm with
        | None -> precompute_rec (pow, ap_gains::unknown) tl
        | Some dBm ->
            let mW = Power.mW dBm in
            precompute_rec (Array.map2 (fun a g -> a +. (mW *. g) ** cfg.norm) pow ap_gains,
                            unknown) tl in
  precompute_rec (Array.make (Array.length samples) 0., []) aps

(* Compute objective J and gradient *)
let objective_and_grad ~norm ~samples ~kernel prior_pow unknown q =
  let unknown_T = Matrix.transpose unknown in
  (* Switch to the power domain *)
  let expq = Array.map exp q in
  (* Predicted mW Pi at sample points i *)
  let p_mW = Array.map (Vector.dot_n norm expq) unknown_T |> Vector.plus prior_pow
             |> Array.map (fun x -> x ** (1. /. norm)) in
  (* Residuals in dB (measurements - prior) *)
  let r = Array.map Power.dBm p_mW |> Vector.minus (Array.map (fun { s_dbm; _ } -> s_dbm) samples) in
  (* Solve α. Kα = r *)
  let alpha = cg_solve kernel r in
  let beta = Array.map2 (fun a p -> -2. *. a *. Util.c10 /. (p ** norm)) alpha p_mW in
  let grad = Array.map2 (fun e gcol -> e ** norm *. Vector.dot_1n norm beta gcol) expq unknown in
  (* J = rᵀK⁻¹r = rᵀα *)
  (Vector.dot r alpha, grad)

(* Merge estimates and known powers *)
let rec merge acc aps q = match (aps, q) with
  | [], _ -> List.rev acc
  | _, [] -> List.rev_append acc aps
  | ({ a_dbm = None; _ } as hd)::tl, hd'::tl' ->
      merge ({ hd with a_dbm = Some (hd' *. Util.c10) }::acc) tl tl'
  | hd::tl, _ -> merge (hd::acc) tl q

(* Calibrate APs of unknown power *)
let calibrate_ap_powers ~cfg ~freq ~walls ~aps ~samples ~kernel =
  if Array.length samples = 0
  then aps
  else
    let (prior_pow, g_unknown) = precompute ~cfg ~freq ~walls ~aps samples in
    if Array.length g_unknown = 0
    then aps
    else
      let q0 = Array.(make (length g_unknown) 0.) in
      gd ~f:(objective_and_grad ~norm:cfg.norm ~samples ~kernel prior_pow g_unknown) q0 50
      |> Array.to_list |> merge [] aps

(** Build the kernel matrix *)
let build_kernel_matrix ~ell ~walls pts jitter =
  let n = Array.length pts in
  Array.init n (fun i ->
    Array.init n (fun j ->
      let k = Math.matern32 (radio_dist ~ell ~walls pts.(i) pts.(j) /. ell) in
      if i = j then k +. jitter else k))

(** Calculate the median distance between all points in a list *)
let median_distance l =
  let rec median_distance_rec acc = function
    | [] -> Math.median acc
    | hd::tl ->
        median_distance_rec ((List.fold_left (fun m p ->
          if p = hd
          then m
          else min m (Point2.dist p hd)) infinity l)::acc) tl in
  median_distance_rec [] l

(** Build a single spectrum predictor *)
let build_single_predictor ~cfg ~freq ~walls ~aps samples =
  let samples = Array.of_list samples in
  let pts = Array.map (fun { s_p; _ } -> s_p) samples in
  let y = Array.map (fun { s_dbm; _ } -> s_dbm) samples in
  let ell = match Array.to_list pts with
    | [] | _::[] -> 1.0
    | l -> median_distance l *. 1.6 in
  let kernel = build_kernel_matrix ~ell ~walls pts (cfg.precision *. cfg.precision) in
  (* Auto-calibrate unknown AP powers *)
  let aps = calibrate_ap_powers ~cfg ~freq ~walls ~aps ~samples ~kernel in
  let prior = Array.map (fun p -> prior_dbm ~exponent:cfg.exponent ~freq ~walls ~aps
                                            ~norm:cfg.norm p) pts in
  let calibration = Vector.minus y prior |> cg_solve kernel in
  predict { calibration; pts; cfg; walls; aps; kernel; ell; freq }

(** Build the model predictor *)
let build_predictor ~cfg { walls; spectrum } =
  let predictors = List.map (fun { s_freq; s_samples; s_aps } ->
    build_single_predictor ~cfg ~freq:s_freq ~walls ~aps:s_aps s_samples) spectrum in
  fun p -> List.fold_left (fun ((power, confidence) as acc) f ->
    let ((power', confidence') as acc') = f p in
    if power' > power || confidence < 0.5 && confidence' > 0.7
    then acc'
    else acc) (neg_infinity, 0.) predictors
