module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string :t->string
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)          
  end

  module type GaussianRationalField =
  sig
    include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
    exception Division_by_zero of string
    val from_rational : (int * int ) -> t   (* rational to complex *)     
    val conj : t -> t                       (* conjugate *)
    val re : t -> (int * int)               (* real part *)
    val im : t -> (int * int)               (* imaginary part *)
  end

module Rationals : RationalField =
  struct
    type t = int * int
    exception Bad_rational of string
    let zero=(0,1)
    let one =(1,1) 

   

      let rec gcd a b =
        if b = 0 then a else gcd b (a mod b);;

    let standard_form (n,d)=
      if d =0 then failwith("Bed rational!!!") else if (n>=0 && d>0) || (n<=0 && d<0)  then  (abs n/abs(gcd n d),abs d/abs(gcd n d )) 
      else if d<0 then ((-1)*(n/abs(gcd n d)),(-1)*d/abs(gcd n d ))  else ( n/abs(gcd n d),d/abs(gcd n d )) 
      
      let to_string (r,i)=
        let (r,i)=standard_form (r,i)in
      
        (string_of_int r)^"/"^(string_of_int i)

      
    let compare (r1,i1) (r2,i2)=
      let (r1,i1)=standard_form (r1,i1)
      in 
      let (r2,i2)=standard_form (r2,i2)
      in

      if r1/i1 = r2/i2 then 0 else if r1/i1 > r2/i2 then 1 else -1 

    let add (r1,i1) (r2,i2)=
      standard_form(r1*i2+i1*r2,i1*i2)
    

    let mul (r1,i1) (r2,i2)=
      standard_form (r1*r2,i1*i2)

    let sub (r1,i1) (r2,i2)=
       standard_form(r1*i2-i1*r2,i1*i2)
    
    let div (r1 ,i1) (r2 , i2) =
         standard_form (r1*i2,i1*r2)
    let add_inv (r1,i2)=
      standard_form (0-r1,i2)


let mul_inv (r1,i1) = if r1 < 0 then  standard_form (i1* -1, r1* -1) else standard_form (i1,r1)
let from_int k =(k,1)
let to_float (r,i) =(float_of_int r) /. (float_of_int i )


  end

  module GaussianRationals : GaussianRationalField =
  struct
    include Rationals
    type t = (int * int) * (int * int)
    exception Division_by_zero of string

    let zero =(0,1),(0,1)
    let one =(1,1),(0,1)
    let add ((r1,i1),(r2,i2)) ((r'1,i'1),(r'2,i'2))=
      ((Rationals.add (r1,i1) (r'1,i'1)),(Rationals.add(r2,i2) (r'2,i'2)))
    let sub ((r1,i1),(r2,i2)) ((r'1,i'1),(r'2,i'2))=
      ((Rationals.sub (r1,i1) (r'1,i'1)),(Rationals.sub (r2,i2) (r'2,i'2)))

     let mul ((r1,i1),(r2,i2)) ((r'1,i'1),(r'2,i'2))=
     ((Rationals.sub (Rationals.mul (r1,i1)(r'1,i'1)) (Rationals.mul(r2,i2)(r'2,i'2))),((Rationals.add (Rationals.mul (r1,i1)(r'2,i'2))(Rationals.mul(r2,i2)(r'1,i'1)))))
    
    let div ((r1,i1),(r2,i2)) ((r'1,i'1),(r'2,i'2))=
Rationals.div(Rationals.add(Rationals.mul(r1,i1)(r'1,i'1))(Rationals.mul(r2,i2)(r'2,i'2)))(Rationals.add(Rationals.mul(r'1,i'1)(r'1,i'1))(Rationals.mul((r'2,i'2))(r'2,i'2))),
Rationals.div(Rationals.sub(Rationals.mul(r2,i2)(r'1,i'1))(Rationals.mul(r1,i1)(r'2,i'2)))(Rationals.add(Rationals.mul(r'1,i'1)(r'1,i'1))(Rationals.mul((r'2,i'2))(r'2,i'2)))
    let add_inv ((r1,i1),(r2,i2))=
      standard_form(0-r1,i1),standard_form(0-r2,i2)

    let mul_inv ((r1,i1),(r2,i2))=
      (Rationals.div (r1,i1)(Rationals.add (Rationals.mul (r1,i1)(r1,i1))(Rationals.mul (r2,i2)(r2,i2)))),Rationals.div(-r2,i2)(Rationals.add (Rationals.mul (r1,i1)(r1,i1))(Rationals.mul (r2,i2)(r2,i2)))

    let im ((r1,i1),(r2,i2))=standard_form(r2,i2)
    let re ((r1,i1),(r2,i2))=standard_form(r1,i1)

    let from_rational (r1,i1) =
      (r1,i1),(0,1)
    let conj ((r1,i1),(r2,i2))=
      (standard_form(r1,i1),standard_form((-r2),i2))

    let compare (r1,i1) (r2,i2)=
      if Rationals.compare r1 r2=0 then Rationals.compare i1 i2 else Rationals.compare r1 r2
    
    let to_string ((r1,i1),(r2,i2))=
    let (r1,i1),(r2,i2)=standard_form (r1,i1),standard_form (r2,i2)in
     let firstname (r1,i1)=
      if i1 =1 then string_of_int r1 else Rationals.to_string (r1,i1)
      in 
      let second (r2,i2)=
        if i2 =1 then string_of_int r2 else Rationals.to_string (r2,i2)
        
      in let outcome =
      if r2<0 then 
      firstname(r1,i1)^second(r2,i2)^"*I" else  firstname(r1,i1)^"+"^second(r2,i2)^"*I"
     
     
      in outcome
      


     

    
end