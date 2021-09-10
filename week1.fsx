
// 1.4
let rec f n =
    match n with
    | 0 -> 0
    | _ -> n + f (n - 1)

// 1.6
let rec sum (m,n) =
    match (m,n) with
    | (m,0) -> m
    | (m,n) -> m + n + sum(m, n-1)

// 2.8
let rec bin (n,k) =
    match (n,k) with
    | (n,0) -> 1
    | (n,k) when n=k -> 1
    | (n,k) -> bin (n-1,k-1) + bin (n-1, k)

// Higher order version
let rec binh n k =
    match n,k with
    | _,0 -> 1
    | n,k when n=k -> 1
    | n,k -> binh (n-1) (k-1) + binh (n-1) k

// 4.7
let rec multicplicity x xs =
    match xs with
    | [] -> 0
    | y::tail when y=x -> 1 + multicplicity x tail
    | _::tail -> multicplicity x tail

