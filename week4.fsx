// 5.3
let rec sum p xs =
  match xs with // Ask why there is incomplete pattern match
  | [] -> 0
  | x::tail when p x -> x + sum p tail
  | x::tail when not (p x) -> sum p tail



// Fold is a top-doown iteration
// It corresponds to a forloop with a mutable variable accumuator

// let sumList list = 
//   List.fold (fun acc elem -> acc + elem) 0 list

let sumList list = 
  List.fold (+) 0 list

// let sumP p x =
//   if p x then x else 0

// let sumPFull p list =
//   List.fold (fun acc x -> acc + (sumP p x)) 0 list

let sumP p list =
  List.fold (fun acc x ->  if p x then acc + x else acc) 0 list

