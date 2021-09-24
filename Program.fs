open System

let rec merge xs ys =
  match xs, ys with
 
  | x::xtail, y::ytail when x<y -> x::merge xtail (y::ytail)
  | x::xtail, y::ytail when y<x -> y::merge (x::xtail) ytail
  | x::xtail, y::ytail -> [y;x]@merge xtail ytail
  | [], ys -> ys
  | xs, [] -> xs




[<EntryPoint>]
let main argv =
    let message = merge [] []

    0 // return an integer exit code