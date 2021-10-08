// Summer 2015 - Problem 2

let f (x) =
  x > 0

let g (x) =
  x < 0

let rec mixMap f xs ys =
  match xs, ys with
  | [],[] -> []
  | x::xtail, y::ytail -> f(x,y)::mixMap f xtail ytail

let unzip zs = List.foldBack
                 (fun (x,y) (xs,ys) -> (x::xs,y::ys))
                 zs
                 ([],[])

let rec unmixMap f g xys =
  let unzipped = unzip xys
  match unzipped with
  | (xs,ys) -> (List.map f xs, List.map g ys)

// Fall 2013 - Problem 1

// 1

let containsNumber number list = List.exists (fun elem -> elem = number) list

let rec isDistinct = function
  | [] -> true
  | x::xs when containsNumber x xs -> false
  | _::xs -> isDistinct xs

let rec isPositive = function
  | [] -> true
  | x::xs when x>0 -> isPositive xs
  | _ -> false

let inv ms =
  let a = List.map fst ms
  let mult = List.map snd ms
  isDistinct a && isPositive mult

// 2
let rec insert e n ms =
  match ms with
  | (e',n')::ms' when e'<>e -> (e',n')::insert e n ms'
  | (e,n')::ms' -> (e,n+n')::ms'
  | [] -> ms @ [(e,n)]

// 3
let rec numberOf e = function
  | [] -> 0
  | (e', n)::ms when e=e' -> n
  | _::ms -> numberOf e ms

// 4
let rec delete e = function
  | (e',n)::ms when e<>e' -> (e',n)::delete e ms
  | (_, n)::ms when n=1 ->  ms
  | (e, n)::ms -> (e, n-1)::ms
  | [] -> []

// 5
let rec union = function
  | ((e,n)::xs, ys) -> union (xs, (insert e n ys))
  | ([], ys) -> ys