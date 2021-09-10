
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

// let evenM n =
//   [for x in 2..2..(n*2) do yield x]

