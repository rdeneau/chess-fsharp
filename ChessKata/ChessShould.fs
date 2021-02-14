module ChessShould

open Chess

open FsUnit
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Reject invalid coordinate`` () =
  raises<exn> <@ emptyGame |> move "xx" "a1" @>
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

  checkWith White "e7" "e5" "not Black's turn to play"
  checkWith Black "e2" "e4" "not White's turn to play"

[<Fact>]
let ``Move piece to the destination square given it's reachable and empty`` () =
  let game = emptyGame |> add '♙' "e2"
  let result = game |> move "e2" "e4"
  result =! Ok (emptyGame |> add '♙' "e4")

[<Fact>]
let ``Reject moving white pawn at e2, e3 or e4 to an empty square not reachable`` () =
  Map.empty
  |> Map.add ("e2": SquareNotation) (allSquareNotations |> List.except ["e2";"e3";"e4"])
  |> Map.add ("e3": SquareNotation) (allSquareNotations |> List.except ["e3";"e4"])
  |> Map.add ("e4": SquareNotation) (allSquareNotations |> List.except ["e4";"e5"])
  |> Map.iter (fun pawnSquare notReachableSquares ->
       notReachableSquares |> List.iter (fun targetSquare ->
         let game = emptyGame |> add '♙' pawnSquare
         let result = game |> move pawnSquare targetSquare
         result =! Error "move not allowed"
       )
     )

[<Fact>]
let ``Reject moving black pawn at e2, e3 or e4 to an empty square not reachable`` () =
  Map.empty
  |> Map.add ("e7": SquareNotation) (allSquareNotations |> List.except ["e7";"e6";"e5"])
  |> Map.add ("e6": SquareNotation) (allSquareNotations |> List.except ["e6";"e5"])
  |> Map.add ("e5": SquareNotation) (allSquareNotations |> List.except ["e5";"e4"])
  |> Map.iter (fun pawnSquare notReachableSquares ->
       notReachableSquares |> List.iter (fun targetSquare ->
         let game = { emptyGame with Turn = Black } |> add '♟' pawnSquare
         let result = game |> move pawnSquare targetSquare
         result =! Error "move not allowed"
       )
     )

[<Fact(Skip = "TODO")>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  let game = emptyGame |> add '♙' "e2"
  let result = game |> move "e2" "e4"
  result =! Ok (emptyGame |> add '♙' "e4")
