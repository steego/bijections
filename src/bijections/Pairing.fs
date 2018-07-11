namespace Pairing

open System.Numerics

module BigInteger = begin

    let sqrt (A:BigInteger) =
        let rec f(x:BigInteger) (maxCount:int) =
            let b = A/x
            let x' = (x + b) / 2I
            let t = abs(x' - x)
            let thresh = abs(x / 100000000I)
            if x = x' + 1I && (x * x) >= A && (x' * x') <= A then x'
            elif x = x' then x'
            elif t < thresh then x'
            elif maxCount <= 0 then x'
            else f x' (maxCount - 1)
        if A = 0I then 0I
        elif A < 4I then 1I 
        else (f (A / 2I) 1000)

end


module NTuples = begin

    // uint64 list -> uint64
    let rec combine (pair: (BigInteger * BigInteger) -> BigInteger) = function
        | [] -> 0I
        | x::[] -> x
        | x::[y] -> pair(y, x)
        | x::rest -> pair(combine pair rest, x)

    // int -> uint64 -> uint64 list
    let rec split unpair size (n:BigInteger) =
        match size with
        | 0 -> []
        | 1 -> [n]
        | 2 -> let (x, y) = unpair(n)
               [y; x]
        | s -> let (x, y) = unpair(n)
               y::(split unpair (s - 1) x)

end

/// A collection of functions use to map the infinite set of natural numbers (Nat)
/// to the infinite set of natural pairs (Nat * Nat)
module CantorB = begin

    open BigInteger

    let pair(k1:BigInteger, k2:BigInteger) = 
        ((k1 + k2) * (k1 + k2 + 1I) / 2I) + k2

    let unpair(z:BigInteger) = 
        let w = ((sqrt(8I * z + 1I) - 1I) / 2I)
        let t = (w * w + w) / 2I
        let y = z - t
        let x = w - y
        (x, y)

    let toList size = NTuples.split unpair size
    let fromList list = NTuples.combine pair list


end

///  Elegant Pairing courtesey of Matthew Szudzik - http://szudzik.com/ElegantPairing.pdf
module Elegant = begin

    open BigInteger

    let pair(x:BigInteger, y:BigInteger) = 
        if max x y <> x then y * y + x
        else (x * x) + x + y

    let unpair(z:BigInteger) = 
        let r = sqrt(z)
        let rl = z - (r * r)
        if rl < r then (rl, r)
        else (r, rl - r)

    let toList size = NTuples.split unpair size
    let fromList list = NTuples.combine pair list

end

module BoundedPair = begin

    let pairMax (maxX:BigInteger) (x:BigInteger, y:BigInteger) = (y * maxX) + x
    let unpairMax (maxX:BigInteger) (n:BigInteger) = 
        let x = n % maxX
        let y = n / maxX
        (x, y)

    module Tests = begin
        let source = [0I..1000I]
        let e1 = source |> List.map (unpairMax 5I)
        let e2 = e1 |> List.map (pairMax 5I)
        let works = (e2 = source)    
    end
    

end

