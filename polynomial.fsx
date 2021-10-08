type Poly = int list

// Part 1

let rec add (xs:Poly) (ys:Poly) =
  match xs,ys with
  | xs,[] -> xs
  | [],ys -> ys
  | x::xtail,y::ytail -> x + y::add xtail ytail

let rec mulC i (xs:Poly) =
  match i, xs with
  | i, _ when i=0 -> []
  | _, [] -> []
  | _, x::tail -> x*i::mulC i tail

let rec sub (xs:Poly) (ys:Poly) =
  match xs,ys with
  | xs,[] -> xs
  | [],ys -> mulC -1 ys
  | x::xtail,y::ytail when x=y && xtail=ytail -> []
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

// Ask about f(+) f(-)

// Part 2

// isLegal
// Auxillary functions
let rec sum xs =
  match xs with
  | [] -> 0
  | x::tail -> x + sum tail

// Poly functions
let rec isLegalShort xs =
  match xs with
  | x::tail when x=0 && ((sum tail) = 0) -> false
  | x::tail when x<>0 && ((sum tail) = 0) -> true
  | [] -> true
  | x::tail -> isLegalShort tail

let rec isLegal xs =
  match xs with
  | [x] when x=0 -> false
  | _::xtail -> isLegal xtail
  | _ -> true

// prune
// Poly functions
let rec prune xs =
  match xs with
  | [] -> []
  | x::tail when not (isLegal(x::tail))  -> []
  | x::tail -> x::prune tail
  // | x::tail when x<>0 && ((sum tail) = 0)  -> [x]


// toString
let int2String x = sprintf "%i" x

let rec toStringAccumulator (xs:list<int>) s n =
  match xs, s, n with
  | [], s, _ -> s
  | x::tail, s, n -> toStringAccumulator tail (s + int2String x + "x^" + int2String n + " ") (n+1) // Must be a better way to format string

let toString xs =
  toStringAccumulator xs "p(x) is " 0

// Derivative
let rec derivativeAccumulator xs n =
  match xs, n with
  | [], _ -> []
  | x::tail, n -> n*x::(derivativeAccumulator tail (n+1))

let derivative xs =
  (derivativeAccumulator xs 0).[1..]

// Compose



// Part 4

type Degree =
  | MinusInf
  | Fin of int

let degree xs =
  match xs with
  | [] -> MinusInf
  | _ -> Fin (List.length xs)

let addD x y =
  match x, y with
  | MinusInf, _ | _, MinusInf -> MinusInf
  | Fin x, Fin y  -> Fin (x + y)

let mulCH i = 
  List.map (fun x -> x * i)


// Part 5

