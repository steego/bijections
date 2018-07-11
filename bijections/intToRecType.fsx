open System

//  int * int -> int
let cantorPair(k1, k2) = (((k1 + k2) * (k1 + k2 + 1)) / 2) + k2

// int -> int * int
let invCantor(z:int) = 
    let z1 = double(z)
    let w = int(Math.Floor((Math.Sqrt(8.0 * z1 + 1.0) - 1.0) / 2.0))
    let t = (w * w + w) / 2
    let y = z - t
    let x = w - y
    (x, y)

let rec cantorTuple = function
    | [] -> 0
    | [x] -> x
    | x::rest -> cantorPair(x, cantorTuple rest)

let rec cantorTupleInv length n =
    match length with
    | 0 -> []
    | 1 -> [n]
    | l -> let (x, y) = invCantor(n)
           x::cantorTupleInv (l - 1) y

[ for i in 0..20 -> cantorTupleInv 3 i |> cantorTuple ]

//////////////////////////////////////

type C = 
    | N of int
    | P of C * C

// let rec width = function
//     | N(_) -> 1
//     | P(x, y) -> width x + width y

// let rec sum = function
//     | N(n) -> n
//     | P(x, y) -> width x + width y

let divR n m = (n % m, n / m)

let rec intToC n = 
    let r = n % 2
    let n = n / 2
    match r with
    | 0 -> N(n)
    | 1 -> let (x, y) = invCantor(n)
           P(intToC x, intToC y)

[ for x in 0..10 -> x, intToC x ] |> List.iter (printfn "%A")


//////////////////////////////////////

let take max n = 
    if n < max then 0, n
    else 1, n

let (|Pair|) n = (invCantor n)
let (|Split|) by n = divR n by

let (|NValue|_|) (max:int) (n:int) =
    if n < max then Some(n)
    else None

let (|NRest|_|) (max:int) (n:int) =
    if n >= max then Some(n - max)
    else None

type BExpr = 
    | Value of bool
    | And of BExpr * BExpr
    | Or of BExpr * BExpr

let rec intToBExpr n =
    match n with
    | NValue 2 n -> Value(n <> 0)
    | NRest 2 (Split 2 (0, Pair(x, y))) -> And(intToBExpr x, intToBExpr y)
    | NRest 2 (Split 2 (1, Pair(x, y))) -> Or(intToBExpr x, intToBExpr y)

let rec BExprToInt = function
    | Value(b) -> if b then 1 else 0
    | And(x, y) -> cantorPair((BExprToInt x), (BExprToInt y)) * 2 + 2
    | Or(x, y) -> cantorPair((BExprToInt x), (BExprToInt y)) * 2 + 3

[ for x in 0..100 -> let e = intToBExpr x in BExprToInt(e), e ] |> List.iter (printfn "%A")