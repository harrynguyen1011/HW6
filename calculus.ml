type poly = int -> float

let rec evaluate (p : poly) (d : int) (x : float) : float =
  match d with 
  | 0 -> p (0)
  | _ -> p (d) *. (x ** float_of_int d) +. evaluate (p) (d - 1) (x)

let rec polynomialEqual (( p : poly ) , ( q : poly ) , ( d :int) ) : bool =
  abs_float ( p d -. q d ) < 0.0001 &&
  match d with
  | 0 -> true
  | _ -> polynomialEqual (p , q , d - 1)

let differentiate (p : poly) : poly = 
  fun n -> float_of_int (n + 1) *. p (n + 1)

let integrate (p : poly) : float -> poly = 
  fun c -> fun n -> 
    match n with
    | 0 -> c
    | _ -> 
      if n > 0 then p (n-1) /. (float_of_int (n)) else 0.0