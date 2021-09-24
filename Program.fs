open System

let rec accumulateTuple xs (ys,zs) =
  match xs with
  | [] -> (ys,zs)
  | [x0] -> (ys@[x0],zs)
  | x0::x1::tail -> accumulateTuple tail (ys@[x0], zs@[x1])

let split xs =
  accumulateTuple xs ([],[])


[<EntryPoint>]
let main argv =
    let message = split [1;2;3;0;0;0]
    0 // return an integer exit code