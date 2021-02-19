module ChessKata.ChessTestHelper

open Chess
open Swensen.Unquote

[<Literal>]
let reachableSymbol = '➕'

let reachableInRank (rankNum: int) (symbols: string) : SquareNotation list =
  let mapReachable (symbol: char, square) =
    if reachableSymbol = symbol then
      Some square.Notation
    else
      None
  symbols
  |> parseRankSymbols rankNum
  |> List.choose mapReachable

/// Test that the piece can move only to the specified reachable squares in the given game.
let testPieceMoveNewV0 pieceSquare (reachableSquares: SquareNotation list) game =
  reachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Ok (game |> reposition pieceSquare targetSquare) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Error "move not allowed" )

let testPieceMove turn pieceSquare (ranks: (int * string) list) =
  let game =
    ranks |> List.fold (fun game (rankNum, symbols) -> game |> addRank rankNum symbols) { emptyGame with Turn = turn }

  let reachableSquares =
    ranks |> List.fold (fun squares (rankNum, symbols) -> squares |> List.append (reachableInRank rankNum symbols)) []

  reachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Ok (game |> reposition pieceSquare targetSquare) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> move pieceSquare targetSquare
       result =! Error "move not allowed" )

let testBlackPieceMove = testPieceMove Black
let testWhitePieceMove = testPieceMove White
