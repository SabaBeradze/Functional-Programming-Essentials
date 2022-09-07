
let hd l = match l with [] -> failwith "invalid" | x::_ -> x
(* or: let hd (x::xs) = x *)

let tl l = match l with [] -> failwith "invalid" | _::xs -> xs
(* or: let tl (x::xs) = xs *)

let rec length = function [] -> 0 | _::xs -> 1 + length xs

let rec append l1 l2 = match l1 with [] -> l2 | x::xs -> x::append xs l2
(* @ *)

let rec rev = function [] -> [] | x::xs -> (rev xs)@[x]

let rev l = 
    let rec impl acc = function [] -> acc | x::xs -> impl (x::acc) xs
    in impl [] l

let rec nth l n = match l with [] -> failwith "invalid" 
    | x::xs -> if n <= 0 then x else nth xs (n-1)


let squaresum1 lst = List.fold_left (fun x y -> x+y*y) 0 lst 

let squaresum2 lst = List.fold_right (fun x y -> x*x+y)  lst  0

let float_list lst = List.map float_of_int lst 

let to_string lst =
"[" ^ (List.fold_left (fun a x -> a ^ (string_of_int x) ^ ";") "" lst) ^ "]"
 


let part_even lst = List.partition ( fun x -> x mod 2 = 0) lst 
