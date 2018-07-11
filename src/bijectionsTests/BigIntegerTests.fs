module BigIntegerTests

open System
open Xunit
open NatSet
open FsUnit
open FsCheck

open Pairing.BigInteger
open System.Numerics

[<Fact>]
let ``The square root of the first 1000 integers squared should be the first 1000 integers`` () =
    let source = [0I..1000I]
    source 
        |> List.map ((fun x -> x * x) >> sqrt)
        |> should equal source


[<Fact>]
let ``Random squares should always be the square root`` () =
    let ``n squared then root equals n`` (n:uint64) = 
        let bi = BigInteger(n)
        (bi * bi) |> sqrt |> uint64 |> should equal n
    
    FsCheck.Check.QuickThrowOnFailure(``n squared then root equals n``)
    
