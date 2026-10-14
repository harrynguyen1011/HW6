(*
Harry Nguyen
Huy Phan
Duc Bui
*)
type poly = int -> float

(*
evaluate : poly -> int -> float -> float
REQUIRES: p is a valid poly (i.e. p n ∼= 0.0 for n < 0 and p n ∼= 0.0 for all but
finitely many n) of degree no more than d (so p n ∼= 0.0 for all n > d)
ENSURES: evaluate p d x | -* - > y, where y is the result of plugging x into polynomial represented by p (not into p itself)
*)
let rec evaluate (p : poly) (d : int) (x : float) : float =
  match d with 
  | 0 -> p (0)
  | _ -> p (d) *. (x ** float_of_int d) +. evaluate (p) (d - 1) (x)

(*
polynomialEqual : poly * poly * int -> bool
REQUIRES: p and q are valid polynomials and d is at least as big as the maximum degree
across p and q
ENSURES: polynomialEqual (p , q , d ) ⇓ true iff p and q are equal as polynomials (within error), and otherwise polynomialEqual (p , q , d ) ⇓ false
*)
let rec polynomialEqual (( p : poly ) , ( q : poly ) , ( d :int) ) : bool =
  abs_float ( p d -. q d ) < 0.0001 &&
  match d with
  | 0 -> true
  | _ -> polynomialEqual (p , q , d - 1)

(*
differentiate : poly -> poly
REQUIRES: p is a valid polynomial
ENSURES: differentiate p ∼= p ’, where p ’ is the derivative of p
*)
let differentiate (p : poly) : poly = 
  fun n -> float_of_int (n + 1) *. p (n + 1)

(*
integrate : poly -> ( float -> poly )
REQUIRES: p is a valid polynomial (i.e. p n ∼= 0.0 for n < 0 and p n ∼= 0.0 for all
but finitely many n)
ENSURES: integrate p ∼= q, where q c is the antiderivative of p with constant of
integration c.
Alternatively, you could say that differentiate (( integrate p ) c ) ∼= p for all
c : float.
*)
let integrate (p : poly) : float -> poly = 
  fun c -> fun n -> 
    match n with
    | 0 -> c
    | _ -> 
      if n > 0 then p (n-1) /. (float_of_int (n)) else 0.0
