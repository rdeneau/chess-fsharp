module ChessShould

open Chess
open Swensen.Unquote
open Xunit

let emptyGame = Game

[<Fact>]
let ``Reject invalid piece coordinate`` () =
  let result = emptyGame |> move "xx" "a1"
  result =! Error "invalid coordinate (piece)"

[<Fact>]
let ``Reject invalid square coordinate`` () =
  let result = emptyGame |> move "a1" "xx"
  result =! Error "invalid coordinate (square)"
