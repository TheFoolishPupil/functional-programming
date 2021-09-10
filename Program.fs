open System

let int2String x = sprintf "%i" x

let rec toStringAccumulator (xs:list<int>) s n =
  match xs, s, n with
  | [], s, _ -> s
  | x::tail, s, n -> toStringAccumulator tail (s + int2String x + "x^" + int2String n + " ") (n+1) // Must be a better way to format string

let toString xs =
  toStringAccumulator xs "p(x) is " 0


[<EntryPoint>]
let main argv =
    let message = toString [1;2;3]
    printfn "%s" message // Call the function
    0 // return an integer exit code