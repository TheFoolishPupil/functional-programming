type Poly = int list

// Part 1

let rec add (xs:Poly) (ys:Poly) =
  match xs,ys with
  | xs,[] -> xs
  | [],ys -> ys
  | x::xtail,y::ytail -> x + y::add xtail ytail

let rec mulC i (xs:Poly) =
  match xs with
  | [] -> []
  | x::tail -> x*i::mulC i tail

let rec sub (xs:Poly) (ys:Poly) =
  match xs,ys with
  | xs,[] -> xs
  | [],ys -> mulC -1 ys
  | x::xtail,y::ytail -> x - y::sub xtail ytail

let mulX (xs:Poly) =
  0::xs

let rec mul (xs:Poly) (ys:Poly) =
  match xs,ys with
  | [],_ -> []
  | x::xtail, ys -> add (mulC x ys) (mulX (mul xtail ys))

let rec eval a (xs:Poly) =
  match xs with
  | [] -> 0
  | x::tail -> x + eval a (mulC a tail)

// Part 2

// Auxillary functions

let rec sum xs =
  match xs with
  | [] -> 0
  | x::tail -> x + sum tail

// Poly functions

let rec isLegal xs =
  match xs with
  | x::tail when x=0 && List.isEmpty tail -> false
  | x::tail when x<>0 && List.isEmpty tail -> true
  | [] -> true
  | x::tail -> isLegal tail

let rec prune xs =
  match xs with
  | [] -> []
  | x::tail when sum tail <> 0  -> x::prune tail
  | x::tail when sum tail = 0 -> [x]