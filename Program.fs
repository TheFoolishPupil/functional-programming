open System

// 2.2
let rec evenN n =
  match n with
  | 0 -> []
  | n when n % 2 <> 0 -> (n-1)*2 :: evenN (n-1)
  | n -> n :: evenN ((n-2)*2)

[<EntryPoint>]
let main argv =
    let message = evenN 10 // Call the function
    0 // return an integer exit code