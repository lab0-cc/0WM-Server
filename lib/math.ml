let clamp ~min ~max v = if v < min then min else if v > max then max else v
let lerp a b t = a +. (b -. a) *. t
let normalize ~min ~max v = clamp ~min:0. ~max:1. ((v -. min) /. (max -. min))

let median = function
  | [] -> invalid_arg "median"
  | l -> match List.length l with
      | n when n mod 2 = 0 -> (Util.quickselect (n / 2 - 1) l +. Util.quickselect (n / 2) l) /. 2.
      | n -> Util.quickselect (n / 2) l

let matern32 d =
  let a = sqrt 3. *. d in
  (1. +. a) *. exp ~-.a
