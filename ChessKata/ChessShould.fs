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

[<Fact>]
let ``Reject moving white pawn at e2, e3 or e4 to an empty square not reachable`` () =
  Map.empty
  |> Map.add ("e2": SquareNotation) (allSquareNotations |> List.except ["e2";"e3";"e4"])
  |> Map.add ("e3": SquareNotation) (allSquareNotations |> List.except ["e3";"e4"])
  |> Map.add ("e4": SquareNotation) (allSquareNotations |> List.except ["e4";"e5"])
  |> Map.add ("e8": SquareNotation) (allSquareNotations |> List.except ["e8"])
  |> Map.iter (fun pawnSquare notReachableSquares ->
       notReachableSquares |> List.iter (fun targetSquare ->
         let game = emptyGame |> add '♙' pawnSquare
         let result = game |> move pawnSquare targetSquare
         result =! Error "move not allowed"
       )
     )

[<Fact>]
let ``Reject moving black pawn at e2, e3 or e4 to an empty square not reachable forward`` () =
  Map.empty
  |> Map.add ("e7": SquareNotation) (allSquareNotations |> List.except ["e7";"e6";"e5"])
  |> Map.add ("e6": SquareNotation) (allSquareNotations |> List.except ["e6";"e5"])
  |> Map.add ("e5": SquareNotation) (allSquareNotations |> List.except ["e5";"e4"])
  |> Map.add ("e1": SquareNotation) (allSquareNotations |> List.except ["e1"])
  |> Map.iter (fun pawnSquare notReachableSquares ->
       notReachableSquares |> List.iter (fun targetSquare ->
         let game = { emptyGame with Turn = Black } |> add '♟' pawnSquare
         let result = game |> move pawnSquare targetSquare
         result =! Error "move not allowed"
       )
     )

let testPieceMove pieceSymbol pieceSquare (reachableSquaresSketch: string) =
  let reachableSquares = split reachableSquaresSketch |> List.except ["xx"]
  reachableSquares
  |> List.iter (fun targetSquare ->
       let game = emptyGame |> add pieceSymbol pieceSquare
       let result = game |> move pieceSquare targetSquare
       result =! Ok (emptyGame |> add pieceSymbol targetSquare) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let game = emptyGame |> add pieceSymbol pieceSquare
       let result = game |> move pieceSquare targetSquare
       result =! Error "move not allowed" )

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
  Map.empty
  |> Map.add ("d4": SquareNotation) (allSquareNotations |> List.except (split
     @"  ;  ;  ;  ;  ;  ;  ;h8;
      ;a7;  ;  ;  ;  ;  ;g7;  ;
      ;  ;b6;  ;  ;  ;f6;  ;  ;
      ;  ;  ;c5;  ;e5;  ;  ;  ;
      ;  ;  ;  ;d4;  ;  ;  ;  ;
      ;  ;  ;c3;  ;e3;  ;  ;  ;
      ;  ;b2;  ;  ;  ;f2;  ;  ;
      ;a1;  ;  ;  ;  ;  ;g1;  ;" ))
  |> Map.iter (fun pawnSquare notReachableSquares ->
       notReachableSquares |> List.iter (fun targetSquare ->
         let game = emptyGame |> add '♗' pawnSquare
         let result = game |> move pawnSquare targetSquare
         result =! Error "move not allowed"
       )
     )

[<Fact(Skip = "TODO")>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  "TODO"

[<Fact(Skip = "TODO")>]
let ``Reject moving pawn to 1-square diagonal occupied by another own piece`` () =
  "TODO"
