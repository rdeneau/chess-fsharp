module ChessShould

open Chess
open FsUnit
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Reject invalid piece coordinate`` () =
  raises<exn> <@ emptyGame |> move "xx" "a1" @>

[<Fact>]
let ``Reject invalid target coordinate`` () =
  raises<exn> <@ emptyGame |> move "a1" null @>

[<Fact>]
let ``Reject no move`` () =
  let result = emptyGame |> move "c3" "c3"
  result =! Error "no move"

[<Fact>]
let ``Reject invalid piece`` () =
  raises<exn> <@ emptyGame |> add '⛃' "e2" @>

[<Fact>]
let ``Reject no piece at the given coordinate`` () =
  let game = emptyGame |> add '♙' "e2"
  let result = game |> move "c3" "c4"
  result =! Error "no piece at c3"

[<Fact>]
let ``Reject moving a piece of a player for which it's not the turn to play`` () =
  let checkWith turn piece destination expectedError =
    let game = { emptyGame with Turn = turn }
               |> add '♙' "e2" // White
               |> add '♟' "e7" // Black
    let result = game |> move piece destination
    result =! Error expectedError

  checkWith White "e7" "e5" "not yet Black's turn"
  checkWith Black "e2" "e4" "not yet White's turn"

[<Fact>]
let ``Move piece to the given destination square`` () =
  let game = emptyGame |> add '♙' "e2"
  let result = game |> move "e2" "e4"
  result =! Ok (emptyGame |> add '♙' "e4")
