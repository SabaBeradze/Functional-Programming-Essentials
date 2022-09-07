let lagrange values =
  let rec build pts' = function [] -> fun x -> 0.0
    | (xi,yi)::xs -> let l = List.fold_left
        (fun f (xj,_) x -> (x -. xj) /. (xi -. xj) *. (f x))
        (fun x -> 1.0) (xs @ pts') in
      let f = build ((xi,yi)::pts') xs in
      fun x -> (yi *. l x) +. f x
  in
  build [] values

(* Approach 2: Using partial application *)
let lagrange values x =
  let l xi = List.fold_left (fun p (xj,yj) ->
    if xi = xj then p else p *. (x -. xj) /. (xi -. xj)) 1. values in
  List.fold_left (fun s (xi,yi) -> s +. (yi *. l xi)) 0. values
