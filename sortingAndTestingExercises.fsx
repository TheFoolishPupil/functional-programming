let rec merge xs ys =
  match xs, ys with
  | x::xtail, y::ytail when x<y -> x::merge xtail (y::ytail)
  | x::xtail, y::ytail when y<x -> y::merge (x::xtail) ytail
  | x::xtail, y::ytail -> [y;x]@merge xtail ytail
  | [], ys -> ys
  | xs, [] -> xs

let rec splitAccumulator xlist (xs, ys) =
  match xlist with
  | x::y::xtail -> splitAccumulator xtail (x::xs, y::ys)
  | [x] -> (x::xs, ys)
  | [] -> (xs, ys)

let rec split xs =
  match xs with
  | _ -> splitAccumulator xs ([], [])

let rec sort xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | xs -> merge (sort (fst (split xs))) (sort (snd (split xs)))

let rec ordered xs =
  match xs with
  | [] -> true
  | [x] -> true
  | x::y::xtail when x<=y -> ordered (y::xtail)
  | _ -> false

#r "nuget: FsCheck, 2.16.03";;
open FsCheck

let orderedSort (xs:int list) = ordered (sort xs)

Check.Quick orderedSort


let rec increment (x, cnt) =
  match cnt with
  | [] -> []
  | (i, c)::ys when i = x -> (i, c+1)::ys
  | y::ys -> y::increment (x, ys)

// let rec toCountingAccumulator xs cnt =
//   match xs, cnt with
//   | (x, c)::xtail,  when

// let toCounting xs =
//   toCountingAccumulator xs []

// Count sums of occurences for each element of an array
let rec toCounting xs =
  match xs with
  | [] -> []
  | x::xtail -> increment (x, toCounting xtail)