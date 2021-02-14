module ChessShould

open Chess

open FsUnit
open Swensen.Unquote
open Xunit

let split (squares: string) : SquareNotation list =
  squares.Split(';')
  |> Array.toList
  |> List.map (fun s -> s.Trim())
  |> List.filter (fun s -> not (System.String.IsNullOrWhiteSpace s))

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

let testPieceMove pieceSymbol pieceSquare (reachableSquaresSketch: string) =
  let { Color = color } = ColoredPiece.parse pieceSymbol
  let emptyGame = { emptyGame with Turn = color }
  let game = emptyGame |> add pieceSymbol pieceSquare

  let reachableSquares = split reachableSquaresSketch |> List.except ["xx"]
  reachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Ok (emptyGame |> add pieceSymbol targetSquare) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Error "move not allowed" )

[<Fact>]
let ``Reject moving white pawn to an empty square not reachable forward`` () =
  testPieceMove '♙' "e2" "e3;e4"
  testPieceMove '♙' "e3" "e4"
  testPieceMove '♙' "e4" "e5"

[<Fact>]
let ``Reject moving black pawn to an empty square not reachable forward`` () =
  testPieceMove '♟' "e7" "e6;e5"
  testPieceMove '♟' "e6" "e5"
  testPieceMove '♟' "e5" "e4"

[<Fact>]
let ``Reject moving knight to an empty square not reachable by jump`` () =
  testPieceMove '♘' "d5"
     @"  ;c7;  ;e7;  ;
      ;b6;  ;  ;  ;f6;
      ;  ;  ;xx;  ;  ;
      ;b4;  ;  ;  ;f4;
      ;  ;c3;  ;e3;  ;"
  testPieceMove '♘' "a1"
     @"  ;c2;  ;
      ;  ;  ;b3;
      ;xx;  ;  ;"

[<Fact>]
let ``Reject moving bishop to an empty square not in diagonal`` () =
  testPieceMove '♗' "d4"
     @"  ;  ;  ;  ;  ;  ;  ;h8;
      ;a7;  ;  ;  ;  ;  ;g7;  ;
      ;  ;b6;  ;  ;  ;f6;  ;  ;
      ;  ;  ;c5;  ;e5;  ;  ;  ;
      ;  ;  ;  ;xx;  ;  ;  ;  ;
      ;  ;  ;c3;  ;e3;  ;  ;  ;
      ;  ;b2;  ;  ;  ;f2;  ;  ;
      ;a1;  ;  ;  ;  ;  ;g1;  ;"

[<Fact>]
let ``Reject moving rook to an empty square not rectilinear`` () =
  testPieceMove '♖' "d5"
     @"  ;  ;  ;d8;  ;  ;  ;  ;
      ;  ;  ;  ;d7;  ;  ;  ;  ;
      ;  ;  ;  ;d6;  ;  ;  ;  ;
      ;a5;b5;c5;xx;e5;f5;g5;h5;
      ;  ;  ;  ;d4;  ;  ;  ;  ;
      ;  ;  ;  ;d3;  ;  ;  ;  ;
      ;  ;  ;  ;d2;  ;  ;  ;  ;
      ;  ;  ;  ;d1;  ;  ;  ;  ;"

[<Fact(Skip = "TODO")>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  "TODO"

[<Fact(Skip = "TODO")>]
let ``Reject moving pawn to 1-square diagonal occupied by another own piece`` () =
  "TODO"
