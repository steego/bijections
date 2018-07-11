module PairingTests

open System
open Xunit
open NatSet
open FsUnit
open FsCheck

open Pairing
open Pairing.BigInteger
open System.Numerics


/// Tests two functions to see if they are inverse functions given x
let testInverse f g = fun x -> 
    let x' = (x |> f |> g) 
    printfn "Inverse Test: %A" (x, x')
    x = x'

let testCantor = testInverse (CantorB.unpair) (CantorB.pair)

// [<Fact>]
// let ``The square root of the first 1000 integers squared should be the first 1000 integers`` () =
//     let source = [0I..1000I]
//     source 
//         |> List.map ((fun x -> x * x) >> sqrt)
//         |> should equal source


[<Fact>]
let ``The Cantor pair and unpair functions should be inverses of each other`` () =
    
    FsCheck.Check.QuickThrowOnFailure(testCantor)
    
