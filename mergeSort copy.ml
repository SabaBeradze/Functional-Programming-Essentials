let rec merge cmp x y = match (x,y) with 
  | ([],_) -> y
  | (_,[]) -> x
  | (h1::t1, h2::t2) -> 
    if cmp h2 h1 
    then h1::(merge cmp t1 y)
    else h2::(merge cmp x t2)

and split x y z = match x with
  | [] -> (y,z)
  | x::resto -> split resto z (x::y)

and  mergesort cmp x = match x with
  | ([] | _::[]) -> x
  | _ -> let (pri,seg) = split x [] [] 
     in (merge cmp (mergesort cmp pri) (mergesort cmp seg));;