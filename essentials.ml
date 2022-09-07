let rec lastelement =function
|[x]->Some x 
|x::xs-> lastelement xs
|[]->None

let rec last2element =function
|[a;b]->Some(a,b)
|x::xs->last2element xs
| []->None

let rec nth  k =function
x::xs->if k=0 then Some x else nth  (k-1) xs
|[]->None

let len l =
  let rec foo  acc =function
  x::xs->foo (acc+1) xs 
  |[]->acc
in foo 0 l 


let rev l =
  let rec foo lst=function
  x::xs->foo  (x::lst) xs
  |[]->lst
in foo [] l

let palindrome l =
   rev l = l 
type 'a node = |One of 'a |Many of 'a node list


(* kargia *)
let rec flaten = function
|One x :: xs -> x:: flaten xs
|Many x::z->flaten x @ flaten z
|[]->[]


let  eliminate l=
  let rec foo  =function
  x::(a::b)-> if x = a  then foo (a::b) else x::foo (a::b)
  |[]->[]
  |b->b
  in foo l

let isempty l =l=[]

let  pack l=
let rec foo l acc = match l with 
x::xs->if isempty acc then foo xs (x::acc) else (
if x=(List.hd acc) then foo xs (x::acc) else (acc)::(foo (x::xs) []))
|x->acc::[]
in (foo l [])

let encode l =
  let temp= pack l
  in 
  let rec foo =function
  x::xs->(List.length x,List.hd x)::foo(xs)
  |[]->[]
  in foo temp

type 'a rle = | One of 'a| Many of int * 'a

let encodebytype l =
  let lst=encode l in
  let rec foo= function
  (x,b)::xs->if x >1 then Many(x,b)::(foo xs) else One(b)::foo xs 
  |[]->[]
  in foo lst

let  decode l=
  let rec foo =function
   Many(n,x)::xs-> if n !=0 then x::foo (Many(n-1,x)::xs)   else foo xs 
  |One(x)::xs->x::foo xs 
  |[]->[]
  in foo l   

let rec dublicate =function
x::xs->x::x::dublicate xs 
|[]->[]


let  replicate  l  n=
  let rec foo n acc=function
x::xs-> if n>0 then x:: foo (n-1) acc (x::xs)  else  foo acc  acc xs 
|[]->[]
in foo n n l
  
let drop l n =
  let rec foo l n acc=match l with
  x::xs->if acc=n then foo xs n 1 else x::(foo xs n (acc+1))
  |[]->[]
in foo l n 1
let splittwoparts l n =
  let rec split l n acc=
    match l with 
    x::xs->if n>0 then split xs (n-1) (acc@[x]) else (acc,xs)
    |[]->([],[])
   in split l n  []

let takeslice i k l =
  let rec foo i k accn accl = function
  |x::xs->if i<=accn && accn<=k then foo i k (accn+1) (accl@[x]) xs else if accn>k then
  accl else foo i k (accn+1) accl xs
  |[]->[]
  in foo i k 0 [] l 

  let  rotate  n l= 
    let rec foo n acc =function
    x::xs-> if n!=0 then foo (n-1) (acc@[x]) xs else x::xs@acc
    |[]->[] 
    in foo n [] l 

  let remove n l =
    let rec foo n l acc=
      match l with x::xs-> if n=0  then acc@xs else foo (n-1) xs (acc@[x])
      |[]->acc
    in foo n l []

let insert el ind l =
  let rec foo el ind acc =function
  x::xs->if ind=0 then acc@([el]@(x::xs)) else foo el (ind-1) (x::acc) xs 
  |[]->l@[el]
  in foo el ind [] l 

  let rec range l r =
    if l<=r then l::range(l+1) r else []

(* this excersise is quite cool *)

(* aritmetic section *)
let  is_prime n =
  let rec foo n acc =
    if n=acc then true else if n mod acc =0 then false else foo n (acc+1 )
    in foo n 2
let gcd  a b  =
  let rec foo a b  =
    if a=b then a else if a>b then foo (a-b) b else foo a (b-a)
    in foo a b 
let coprime a b  =
   gcd a b =1 

let factors a =
  let rec foo a acc =
    if a mod acc =0 &&  a>1 then acc :: (foo (a/acc) 2) else if a=1 then [] else  foo a (acc+1)
    in foo a 2
let rec contains  a =function
x::xs-> if x= a then true else contains a xs
|[]->false
  
let number_prime_factors l =
  let rec eliminatedublicate lst acc =
    match lst with a::xs->
    if ( contains  a acc  ) then eliminatedublicate xs (acc) else eliminatedublicate xs (acc@[a])
    |[]->acc
    in 
    let rec count el l=
      match l with x::xs-> if x=el then 1+count el xs else count el xs
      |[]->0
      in
      let rec foo lst l =
        match lst with x::xs->(x,count x l)::foo xs l 
        |[]->[]
        in foo (eliminatedublicate l []) l
let all_primes a =
  let rec foo a b =
    if is_prime b && a>b then b::(foo a (b+1)) else if a=b then if is_prime b then[b]else [] else
    foo a (b+1)
    in foo a 2 
  let gold_bach a =
  let rec  goldbach a b =
    if is_prime a &&  is_prime b then (a,b) else goldbach(a+1)(b-1)
in goldbach 2 (a-2)

  let rec in_range_gold_bach a b=
    if is_prime a then in_range_gold_bach (a+1) b else 
 if a=b then [] else (a,gold_bach a )::in_range_gold_bach (a+1) b


  type bool_expr =
 | Var of string
 | Not of bool_expr
 | And of bool_expr * bool_expr
 | Or of bool_expr * bool_expr;;

 let rec contains l a =
  match l with x::xs->if x=a then true else contains xs a
  |[]->false

let  eliminate l =
  let rec foo l acc =
    match l with x::xs-> if contains acc x then foo xs acc else foo xs  (acc@[x])
    |_->acc
    in foo l []
 


