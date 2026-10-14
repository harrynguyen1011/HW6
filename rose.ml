(* 
Huy Phan
Harry Nguyen
Duc Bui   
*)

type 'a rose = Rose of 'a * 'a rose list

(* Task 4.1
 * roseMap : ('a -> 'b) -> 'a rose -> 'b rose
 * REQUIRES: true
 * ENSURES: rosePreord (roseMap f r) =~ List.map f (rosePreord r)
 *)
let rec roseMap f r =
  match r with
  | Rose (x, children) ->
    let tmp = f x in
    Rose (tmp, List.map (roseMap f) children)

(* Task 4.2
 * roseFilter : ('a -> bool) -> 'a rose -> 'a rose option
 * REQUIRES: true
 * ENSURES: If roseFilter p r =~ None, then
 *            List.filter p (rosePreord r) =~ [].
 *          If roseFilter p r =~ Some r', then
 *            rosePreord r' =~ List.filter p (rosePreord r).
 *)
let rec roseFilter p r =
  match r with
  | Rose (x, children) ->
    let keep = p x in
    let children' = roseFilterChildren p children in
    if keep
    then Some (Rose (x, children'))
    else
      match children' with
      | [] -> None
      | Rose (y, ys) :: rest -> Some (Rose (y, ys @ rest))

and roseFilterChildren p children =
  match children with
  | [] -> []
  | child :: rest ->
    let tmp = roseFilter p child in
    let rest' = roseFilterChildren p rest in
    match tmp with
    | None -> rest'
    | Some child' -> child' :: rest'

(* Task 4.3
 * roseFoldLeft : ('acc -> 'a -> 'acc) -> 'acc -> 'a rose -> 'acc
 * REQUIRES: true
 * ENSURES: roseFoldLeft f z r =~ List.fold_left f z (rosePreord r)
 *)
let rec roseFoldLeft f z r =
  match r with
  | Rose (x, children) ->
    let tmp = f z x in
    List.fold_left (roseFoldLeft f) tmp children

(* Task 4.4
 * roseFoldRight : ('a -> 'acc -> 'acc) -> 'a rose -> 'acc -> 'acc
 * REQUIRES: true
 * ENSURES: roseFoldRight f r z =~ List.fold_right f (rosePreord r) z
 *)
let rec roseFoldRight f r z =
  match r with
  | Rose (x, children) ->
    let tmp = List.fold_right (roseFoldRight f) children z in
    f x tmp
