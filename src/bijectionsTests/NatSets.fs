module NatSets.Tests

open System
open Xunit
open NatSet

// [<Fact>]
// let ``Unbounded integers have infinite cardinality`` () =
//     Assert.Same(Infinite, AllIntegers |> getSetSize)

let (===) x y = Assert.StrictEqual<_>(x,y)

[<Fact>]
let ``Bounded integers have a finite cardinality`` () =
    let thirtyInts = Bounded(30UL)
    30UL === (thirtyInts |> getSetSize)
    