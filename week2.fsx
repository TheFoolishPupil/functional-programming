
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
// Test case
let p x =
  x > 0


let rec sum p xs =
  match xs with // Ask why there is incomplete pattern match
  | [] -> 0
  | x::tail when p x -> x + sum p tail
  | x::tail when not (p x) -> sum p tail





// 4.16

// int * list<int> -> list<int>
// let rec f = function
//     | (x, [])    -> []
//     | (x, y::ys) -> (x+y)::f(x-1, ys);;

// list<a' * a'> -> list<a' * a'>
// let rec g = function
//     | []       -> []
//     | (x,y)::s -> (x,y)::(y,x)::g s;;

// list<a'> -> list<a'>
// let rec h = function 
//   |[] -> []
//   | x::xs -> x::(h xs)@[x];;

