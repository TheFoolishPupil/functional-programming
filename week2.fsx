
// 2.1
let f n =
  match n with
  | n when n % 5 = 0 -> false
  | n when n % 3 = 0 -> true
  | n when n % 2 = 0 -> true
  | _ -> false

// 2.2
let rec pow (s,n) =
  match (s,n) with
  | (s,0) -> ""
  | (s,n) -> s + pow(s, n-1)

// 2.13
// Curry f, returns a higher order function (fun a -> fun b) that computes the same value as the uncurried 'f (a,b)'
let curry f = 
  fun a -> fun b -> f (a,b)

// Uncurry f, returns a function of a pair, that computes the same value as the higher order function 'f a b'
let uncurry f = 
  fun (a,b) -> f a b

// 4.3
let rec evenN n =
  match n with
  | 0 -> []
  | n -> evenN(n-1) @ [(n*2)]

// Using sequence
let evenM n =
  [for x in 2..2..(n*2) do yield x]

// 4.8
let rec accumulateTuple xs (ys,zs) =
  match xs with
  | [] -> (ys,zs)
  | [x0] -> (ys@[x0],zs)
  | x0::x1::tail -> accumulateTuple tail (ys@[x0], zs@[x1])

let split xs =
  accumulateTuple xs ([],[])

// 4.9
let rec zip (xs,ys) =
  match (xs,ys) with
  | ([],[]) -> []
  | (_,[]) -> [] // Annoying
  | ([],_) -> [] // Annoying
  | (x0::xtail,y0::ytail) -> (x0,y0)::zip (xtail,ytail)

// 4.12
let p x =
  x > 0

let accumulateSum p xs sum =
  match p,xs,sum with
  | _,[],sum -> p sum

let sum p xs =
  accumulateSum p xs 0

// 4.16

let rec f = function
    | (x, [])    -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys);;

let rec g = function
    | []       -> []
    | (x,y)::s -> (x,y)::(y,x)::g s;;
    
let rec h = function 
  |[] -> []
  | x::xs -> x::(h xs)@[x];;