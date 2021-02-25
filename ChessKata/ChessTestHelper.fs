module ChessKata.ChessTestHelper

open Chess
open Swensen.Unquote

let emptyGame = { Board = Map.empty; Turn = White }

let square = Square.parse

let parseRankSymbols (rankNum: int) (symbols: string) =
  let rank = rankNum |> int |> enum<Rank>
  let mapSymbol (symbol, file) =
    let square = Square.create file rank
    (symbol, square)
  match symbols |> Seq.toList with
  | [a; b; c; d; e; f; g; h] ->
    [
      (a, File.a)
      (b, File.b)
      (c, File.c)
      (d, File.d)
      (e, File.e)
      (f, File.f)
      (g, File.g)
      (h, File.h)
    ]
    |> List.map mapSymbol
  | _ -> failwith "invalid symbols"

let addRank (rankNum: int) (symbols: string) (game: Game) : Game =
  let mapSymbol (symbol, square) =
    symbol
    |> ColoredPiece.tryParse
    |> Option.map (fun piece -> (piece, square))
  let board =
    symbols
    |> parseRankSymbols rankNum
    |> List.choose mapSymbol
    |> List.fold (fun board (piece, square) -> board |> Map.add square piece) game.Board
  { game with Board = board }

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
       let result = game |> Game.movePiece pieceSquare targetSquare
       result =! Ok (game |> Game.reposition pieceSquare targetSquare) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> Game.movePiece pieceSquare targetSquare
       result =! Error "move not allowed" )

let testPieceMove turn pieceSquare (ranks: (int * string) list) =
  let game =
    ranks |> List.fold (fun game (rankNum, symbols) -> game |> addRank rankNum symbols) { emptyGame with Turn = turn }

  let reachableSquares =
    ranks |> List.fold (fun squares (rankNum, symbols) -> squares |> List.append (reachableInRank rankNum symbols)) []

  reachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> Game.movePiece pieceSquare targetSquare
       result =! Ok (game |> Game.reposition pieceSquare targetSquare |> Game.toggleTurn) )

  let notReachableSquares = allSquareNotations |> List.except (pieceSquare::reachableSquares)
  notReachableSquares
  |> List.iter (fun targetSquare ->
       let result = game |> Game.movePiece pieceSquare targetSquare
       result =! Error "move not allowed" )

let testBlackPieceMove = testPieceMove Black
let testWhitePieceMove = testPieceMove White
