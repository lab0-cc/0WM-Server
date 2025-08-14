[%%marshal.load Json]

type point = { p_x: float [@json "x"]; p_y: float [@json "y"] } [@@marshal]
type vector = { v_x: float; v_y: float }
type matrix = { m_a: float; m_b: float; m_c: float; m_d: float }
type segment = { s_p1: point; s_p2: point; s_v: vector }

let vec p p' = { v_x = p'.p_x -. p.p_x; v_y = p'.p_y -. p.p_y }

let det v v' = v.v_x *. v'.v_y -. v.v_y *. v'.v_x

let dot v v' = v.v_x *. v'.v_x +. v.v_y *. v'.v_y

let norm v = dot v v |> Float.sqrt

let app { m_a; m_b; m_c; m_d } { p_x; p_y } =
  { p_x = m_a *. p_x +. m_b *. p_y; p_y = m_c *. p_x +. m_d *. p_y }

let tran { v_x; v_y } { p_x; p_y } = { p_x = p_x +. v_x; p_y = p_y +. v_y }

let scale { v_x; v_y } s = { v_x = v_x *. s; v_y = v_y *. s }

let seg s_p1 s_p2 = { s_p1; s_p2; s_v = vec s_p1 s_p2 }

let psclosest p { s_p1; s_p2; s_v } =
  let pos = dot (vec s_p1 p) s_v /. dot s_v s_v in
  if pos <= 0. then s_p1 else if pos >= 1. then s_p2 else tran (scale s_v pos) s_p1
