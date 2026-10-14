type token =
  | Integer of int
  | Multiply
  | Add
  | Subtract
  | Divide
  | SumAll

let tokenize ( t : string ) : token =
  match t with
  | "*" -> Multiply
  | "+" -> Add
  | "-" -> Subtract
  | "/" -> Divide
  | "sum" -> SumAll
  | _ -> match int_of_string_opt t with
  | Some n -> Integer n
  | None -> failwith (" invalid token : " ^ t )

let (%) ( f : 'a -> 'b ) ( g : 'c -> 'a ) ( x : 'c ) : 'b = f ( g x )
let parse (s : string) : token list =
  let s_list = String.split_on_char (' ') (s) in
  let s_processed = List.filter (fun c -> c <> "") (s_list) in
  List.map (tokenize) (s_processed)

let eval (tokens : token list) : int list =
  let step (stack : int list) (t : token) : int list =
    match t, stack with 
    | Integer n, _ -> n :: stack
    | Multiply, b :: a :: stack -> (a * b) :: stack
    | Add, b :: a :: stack -> (a + b) :: stack
    | Subtract, b :: a :: stack -> (a - b) :: stack
    | Divide, b :: a :: stack -> (a / b) :: stack
    | SumAll, _ -> [List.fold_left (+) 0 stack]
    | (Multiply | Add | Subtract | Divide), _ -> failwith ("Not Enough Operands")
  in List.fold_left step [] tokens


let rpn (s : string) : int list = 
  let token_list = parse (s) in 
  eval (token_list)