[%%marshal.load Json]

(** The type of 2D points *)
type point = { p_x: float [@json "x"]; p_y: float [@json "y"] } [@@marshal]

(** The type of 3D points *)
type point3 = { p3_x: float [@json "x"]; p3_y: float [@json "y"]; p3_z: float [@json "z"] } [@@marshal]

module Vector2 = struct
  (** Get the vector from the first point to the second *)
  let of_points p p' = { p_x = p'.p_x -. p.p_x; p_y = p'.p_y -. p.p_y }

  (** 2D cross product (or 2×2 matrix determinant) *)
  let cross v v' = v.p_x *. v'.p_y -. v.p_y *. v'.p_x

  (** Scale a vector *)
  let scaled s { p_x; p_y } = { p_x = s *. p_x; p_y = s *. p_y }

  (** Add a vector to a point *)
  let plus p v = { p_x = p.p_x +. v.p_x; p_y = p.p_y +. v.p_y }

  (** Subtract a vector from a point *)
  let minus p v = { p_x = p.p_x -. v.p_x; p_y = p.p_y -. v.p_y }

  (** Vector dot product *)
  let dot p p' = p.p_x *. p'.p_x +. p.p_y *. p'.p_y

  (** Get the vector square norm *)
  let sqnorm p = dot p p

  (** Get the vector norm *)
  let norm p = sqnorm p |> sqrt

  (** Negate a vector *)
  let neg { p_x; p_y } = { p_x = -.p_x; p_y = -.p_y }
end

module Point2 = struct
  (** Get the square distance between two points *)
  let sqdist p p' = Vector2.(of_points p p' |> sqnorm)

  (** Get the distance between two points *)
  let dist p p' = Vector2.(of_points p p' |> norm)

  (** Interpolate between two points *)
  let lerp p p' t = { p_x = Math.lerp p.p_x p'.p_x t; p_y = Math.lerp p.p_y p'.p_y t }
end

module Segment2 = struct
  (** The type of 2D segments *)
  type t = { s_start: point; s_end: point }

  (** Get the segment from the first point to the second *)
  let of_points s_start s_end = { s_start; s_end }

  (** The type of segment intersections *)
  type intersection = Parallel | Outside | Inside of point

  (** Get the vector from the first point to the second *)
  let vec { s_start; s_end } = Vector2.of_points s_start s_end

  (** Intersect two segments *)
  let intersect s s' =
    let v = vec s in
    let v' = vec s' in
    let den = Vector2.cross v v' in
    if abs_float den < Util.eps
    then Parallel
    else
        let v'' = Vector2.(of_points s.s_start s'.s_start |> scaled (1. /. den)) in
        let factor = Vector2.cross v'' v' in
        let factor' = Vector2.cross v'' v in
        if factor < 0. || factor > 1. || factor' < 0. || factor' > 1.
        then Outside
        else Inside Vector2.(scaled factor v |> plus s.s_start)

  (** Check if two segments cross *)
  let crosses s s' = match intersect s s' with
    | Inside _ -> true
    | _ -> false

  (** Mirror a point *)
  let mirror_p p ({ s_start; _ } as s) =
    let v = vec s in
    let v' = Vector2.of_points s_start p in
    let t = Vector2.(dot v v' /. sqnorm v) in
    Vector2.(scaled (t *. 2.) v |> plus (minus s_start v'))

  (** Get the closest point on a segment *)
  let closest p ({ s_start; _ } as s) =
    let v = vec s in
    let t = Math.clamp ~min:0. ~max:1. (Vector2.((of_points s_start p |> dot v) /. sqnorm v)) in
    Vector2.(scaled t v |> plus s_start |> minus p)

  (** Get the square distance between a point and a segment *)
  let sqdist_p p s = closest p s |> Vector2.sqnorm
end

module Box2 = struct
  (** The type of 2D boxes *)
  type t = { b_min: point [@json "min"]; b_max: point [@json "max"] } [@@marshal]

  (** Get the bounding box from a list of points *)
  let of_points = function
    | [] -> { b_min = { p_x = 0.; p_y = 0. }; b_max = { p_x = 0.; p_y = 0. } }
    | hd::tl ->
        List.fold_left
          (fun { b_min; b_max } p ->
             { b_min = { p_x = min b_min.p_x p.p_x; p_y = min b_min.p_y p.p_y };
               b_max = { p_x = max b_max.p_x p.p_x; p_y = max b_max.p_y p.p_y } })
          { b_min = hd; b_max = hd } tl

  (** Get the diagonal vector from the given box *)
  let vec { b_min; b_max } = Vector2.of_points b_min b_max

  (** Expand a box by a padding *)
  let expand padding { b_min; b_max } =
    { b_min = { p_x = b_min.p_x -. padding; p_y = b_min.p_y -. padding };
      b_max = { p_x = b_max.p_x +. padding; p_y = b_max.p_y +. padding } }
end

module Matrix3 = struct
  (** The type of 3×3 matrices *)
  type t = { m_a: float; m_b: float; m_c: float;
             m_d: float; m_e: float; m_f: float;
             m_g: float; m_h: float; m_i: float }

  (** Identity matrix *)
  let one = { m_a = 1.; m_b = 0.; m_c = 0.;
              m_d = 0.; m_e = 1.; m_f = 0.;
              m_g = 0.; m_h = 0.; m_i = 1. }

  (** Build a matrix from a translation vector *)
  let of_translation v = { one with m_c = v.p_x; m_f = v.p_y }

  (** Build a matrix from a scaling factor *)
  let of_scale s = { one with m_a = s; m_e = s }

  (** Apply a matrix to a point *)
  let apply_p2 { p_x; p_y } m =
    { p_x = m.m_a *. p_x +. m.m_b *. p_y +. m.m_c; p_y = m.m_d *. p_x +. m.m_e *. p_y +. m.m_f }

  (** Apply a matrix to a vector *)
  let apply_v2 { p_x; p_y } m =
    { p_x = m.m_a *. p_x +. m.m_b *. p_y; p_y = m.m_d *. p_x +. m.m_e *. p_y }

  (** Apply a matrix to a segment *)
  let apply_s2 Segment2.{ s_start; s_end } m =
    Segment2.{ s_start = apply_p2 s_start m; s_end = apply_p2 s_end m }

  (** Multiply two matrices *)
  let mul m m' =
    { m_a = m.m_a *. m'.m_a +. m.m_b *. m'.m_d +. m.m_c *. m'.m_g;
      m_b = m.m_a *. m'.m_b +. m.m_b *. m'.m_e +. m.m_c *. m'.m_h;
      m_c = m.m_a *. m'.m_c +. m.m_b *. m'.m_f +. m.m_c *. m'.m_i;
      m_d = m.m_d *. m'.m_a +. m.m_e *. m'.m_d +. m.m_f *. m'.m_g;
      m_e = m.m_d *. m'.m_b +. m.m_e *. m'.m_e +. m.m_f *. m'.m_h;
      m_f = m.m_d *. m'.m_c +. m.m_e *. m'.m_f +. m.m_f *. m'.m_i;
      m_g = m.m_g *. m'.m_a +. m.m_h *. m'.m_d +. m.m_i *. m'.m_g;
      m_h = m.m_g *. m'.m_b +. m.m_h *. m'.m_e +. m.m_i *. m'.m_h;
      m_i = m.m_g *. m'.m_c +. m.m_h *. m'.m_f +. m.m_i *. m'.m_i }
end

module Vector = struct
  (** The type of n-dimensional vectors *)
  type t = float array

  (** Vector dot product *)
  let dot v v' = Array.(combine v v' |> fold_left (fun acc (x, x') -> acc +. x *. x') 0.)

  (** v.v'^n dot product *)
  let dot_1n n v v' = Array.(combine v v' |> fold_left (fun acc (x, x') -> acc +. x *. x' ** n) 0.)

  (** v^n.v'^n dot product *)
  let dot_n n v v' = Array.(combine v v' |> fold_left (fun acc (x, x') -> acc +. (x *. x') ** n) 0.)

  (** Add a vector to a point *)
  let plus = Array.map2 (+.)

  (** Subtract a vector from a point *)
  let minus = Array.map2 (-.)

  (** Get the vector square norm *)
  let sqnorm = Array.fold_left (fun acc x -> acc +. x *. x) 0.

  (** Scale a vector *)
  let scaled s = Array.map (( *. ) s)
end

module Matrix = struct
  (** The type of n×n matrices *)
  type t = Vector.t array

  (** Build a matrix from a list of vectors *)
  let of_vectors = Array.of_list

  (** Transpose a matrix *)
  let transpose m =
    match Array.length m with
    | 0 -> [||]
    | x ->
        let y = Array.length m.(0) in
        Array.init y (fun i -> Array.init x (fun j -> m.(j).(i)))

  (** Apply the transpose of the given matrix to the given vector *)
  let applyT_v v =
    Array.map (Vector.dot v)

  (** Apply a matrix to a vector *)
  let apply_v v m =
    transpose m |> applyT_v v
end

(** Conjugate gradient solve for SPD matrix *)
let cg_solve m v =
  let sqtol = 1e-8 in
  let max_iter = max 20 (5 * Array.length v) in
  let rec cg_solve_rec i x r p sq =
    if i >= max_iter || sq <= sqtol
    then x
    else
      let mp = Matrix.apply_v p m in
      let w = sq /. Vector.dot p mp in
      let r' = Vector.(scaled w mp |> minus r) in
      let sq' = Vector.sqnorm r' in
      cg_solve_rec (i + 1) Vector.(scaled w p |> plus x) r' Vector.(scaled (sq' /. sq) p |> plus r')
                   sq' in
  let x = Array.make (Array.length v) 0. in
  let r = Matrix.apply_v x m |> Vector.minus v in
  Vector.sqnorm r |> cg_solve_rec 0 x r r

(** Gradient descent *)
let rec gd ~f q = function
  | 0 -> q
  | i ->
      let (j, g) = f q in
      let sq = Vector.sqnorm g in
      if sq < 1e-6
      then q
      else
        let rec backtrack eta i =
          let q' = Vector.(scaled eta g |> minus q) in
          let (j', _) = f q' in
          if j' <= j -. 1e-4 *. eta *. sq || i = 0
          then (q', j')
          else backtrack (eta /. 2.) (i - 1) in
        let (q, j') = backtrack 1.0 10 in
        if j -. j' < 1e-6 *. j
        then q
        else gd ~f q (i - 1)
