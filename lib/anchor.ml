[%%marshal.load Json]

type t = { a_x: float [@json "x"]; a_y: float [@json "y"];
           a_lng: float [@json "lng"]; a_lat: float [@json "lat"] } [@@marshal]

let to_points a = Linalg.({ p_x = a.a_x; p_y = a.a_y }, { p_x = a.a_lng; p_y = a.a_lat })

type set = t * t * t [@@marshal]
