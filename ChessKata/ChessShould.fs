module ChessShould

open Chess
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Reject invalid square coordinate`` () =
    Game
    |> move "a1" "xx"
    |> should equal (Error "invalid square coordinate")