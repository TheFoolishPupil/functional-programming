open System



let unzip zs = List.foldBack
                 (fun (x,y) (xs,ys) -> (x::xs,y::ys))
                 zs
                 ([],[])



[<EntryPoint>]
let main argv =
    // let message = increment (2, [(1,1); (2,4); (3,1)])
    let x = unzip [(1,2);(3,4)]
    0 // return an integer exit code