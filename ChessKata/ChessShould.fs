module ChessShould

open Chess
open FsUnit
open Swensen.Unquote
open Xunit

let emptyGame () = { Board = Map.empty; Turn = White }

[<Fact>]
let ``Reject invalid piece coordinate`` () =
  let act () = emptyGame () |> move "xx" "a1" |> ignore
  act |> shouldFail

[<Fact>]
let ``Reject invalid target coordinate`` () =
  let act () = emptyGame () |> move "a1" null |> ignore
  act |> shouldFail

[<Fact>]
let ``Reject no move`` () =
  let result = emptyGame () |> move "c3" "c3"
  result =! Error "no move"

[<Fact>]
let ``Reject invalid piece`` () =
  let act () = emptyGame () |> add "X" "e2" |> ignore
  act |> shouldFail

[<Fact>]
let ``Reject no piece at the given coordinate`` () =
  let game = emptyGame () |> add "♙" "e2"
  let result = game |> move "c3" "c4"
  result =! Error "no piece at c3"

[<Fact>]
let ``Move piece to the given destination square`` () =
  let game = emptyGame () |> add "♙" "e2"
  let result = game |> move "e2" "e4"
  result =! Ok (emptyGame () |> add "♙" "e4")

// TODO: reject / turn
