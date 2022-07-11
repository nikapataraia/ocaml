module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
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
    val im : t -> (int * int)               (* inaginary part *)
  end
module Rationals : RationalField =
      struct
        type t = int * int
        exception Bad_rational of string
        let zero = (0,1)
        let one = (1,1)
        let standard_form (n,d) = 
          let rec helper a b c = 
            match c with 1 -> (a,b) 
             |_ -> if((a mod c = 0)&&(b mod c = 0)) then helper (a/c) (b/c) (b/c) else helper a b (c-1) in
            if d = 0 then raise (Bad_rational "denominator = 0") else
            if d < 0 then helper (-n) (-d) (-d) else helper n d d
        let add (r1,i1) (r2,i2) = 
          standard_form (r1 * i2 + r2 * i1 , i1 * i2)
        let from_int a = (a,1)
        let to_float (a,b)= float_of_int a /. float_of_int b
        let mul_inv (a,b) = standard_form(b,a)
        let add_inv (a,b) = standard_form(-a,b)
        let sub (a,b) (c,d) = standard_form (a * d - b * c , b * d)
        let mul (a,b) (c,d) = standard_form (a*c,b*d)
        let div (a,b) (c,d) = if c = 0 then raise (Bad_rational "cant devide by zero") else mul (a,b) (d,c)
        let rec to_string (a,b) = if((a,b) = standard_form (a,b)) then String.concat "/" [string_of_int a; string_of_int b] else to_string (standard_form (a,b))
        let compare (r1,i1) (r2,i2) = 
          let a1 = standard_form (r1,i1) in
          let a2 = standard_form (r2,i2) in
          compare (to_float a1) (to_float a2)
      end


module GaussianRationals : GaussianRationalField =
      struct
        type t = (int * int) * (int * int)
        exception Division_by_zero of string
        let zero = (0,1),(0,1)
        let one = (1,1),(0,1)

        let compare (r1,i1) (r2,i2) = 
          (* Rationals.compare  (Rationals.add (Rationals.mul r1 r1) (Rationals.mul i1 i1)) (Rationals.add (Rationals.mul r2 r2) (Rationals.mul i2 i2)) *)
          if Rationals.compare r1 r2 = 0 then Rationals.compare i1 i2 else Rationals.compare r1 r2

        

        let rec to_string (r,i) = if(r = Rationals.standard_form r && i = Rationals.standard_form i) 
        then if(let (a,b) = i in a < 0) then String.concat "" [Rationals.to_string r;(Rationals.to_string  i) ^ "*I"] else String.concat "+" [Rationals.to_string r;(Rationals.to_string  i) ^ "*I"]
         else to_string (Rationals.standard_form r,Rationals.standard_form i)
        

        let from_rational r = (r,(1,1))
        let add (a,b) (c,d) = (Rationals.add a c),(Rationals.add b d)
        let mul (a,b) (c,d) = (Rationals.sub (Rationals.mul a c) (Rationals.mul b d)),(Rationals.add (Rationals.mul b c) (Rationals.mul a d))
        let sub (a,b) (c,d) = (Rationals.sub a c),(Rationals.sub b d)
        let mul_inv (a,b) = if (Rationals.standard_form a = Rationals.zero && Rationals.standard_form b = Rationals.zero) then raise (Division_by_zero "cant invert") 
         else (Rationals.div (a) (Rationals.add (Rationals.mul a a) (Rationals.mul b b)),Rationals.add_inv (Rationals.div (b) (Rationals.add (Rationals.mul a a) (Rationals.mul b b))))
        let div (a,b) (c,d) = mul (a,b) (mul_inv (c,d))
        let re (a,b) = Rationals.standard_form a
        let im (a,b) = Rationals.standard_form b
        let conj (a,b) = (a,Rationals.add_inv b)
        let add_inv (a,b) = (Rationals.add_inv a,Rationals.add_inv b)
          
        
       end;;
