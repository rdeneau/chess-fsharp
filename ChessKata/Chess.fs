module Chess

open FSharpPlus
open Helpers
open System

type File = a = 1 | b = 2 | c = 3 | d = 4 | e = 5 | f = 6 | g = 7 | h = 8 // Column
type Rank = _1 = 1 | _2 = 2 | _3 = 3 | _4 = 4 | _5 = 5 | _6 = 6 | _7 = 7 | _8 = 8 // Row
type SquareNotation = string // E.g. "a1"
type Square = { Notation: SquareNotation; File: File; Rank: Rank }
type Color = Black | White
type Piece = King | Queen | Rook | Bishop | Knight | Pawn
type ColoredPiece = { Color: Color; Piece: Piece }
type PieceSymbol = char
type Game = { Board: Map<Square, ColoredPiece>; Turn: Color }

let emptyGame = { Board = Map.empty; Turn = White }

module ColoredPiece =
  let parse (piece: PieceSymbol) : ColoredPiece =
    match piece with
    | '♔' -> { Color = White; Piece = King }
    | '♕' -> { Color = White; Piece = Queen }
    | '♖' -> { Color = White; Piece = Rook }
    | '♗' -> { Color = White; Piece = Bishop }
    | '♘' -> { Color = White; Piece = Knight }
    | '♙' -> { Color = White; Piece = Pawn }
    | '♚' -> { Color = Black; Piece = King }
    | '♛' -> { Color = Black; Piece = Queen }
    | '♜' -> { Color = Black; Piece = Rook }
    | '♝' -> { Color = Black; Piece = Bishop }
    | '♞' -> { Color = Black; Piece = Knight }
    | '♟' -> { Color = Black; Piece = Pawn }
    | s -> failwith $"invalid piece {s}"

module Square =
  let parse (notation: SquareNotation) =
    match notation with
    | Regex @"([a-h])([1-8])" [ file; rank ] ->
      {
        Notation = notation
        File     = Enum.Parse(typeof<File>, file) :?> File
        Rank     = rank |> int |> enum<Rank>
      }
    | _ -> failwith "invalid coordinate"

let add (piece: PieceSymbol) (square: SquareNotation) (game: Game) : Game =
  let board = game.Board |> Map.add (square |> Square.parse) (piece |> ColoredPiece.parse)
  { game with Board = board }

let move (pieceLocation: SquareNotation) (targetLocation: SquareNotation) (game: Game) : Result<Game, string> =

  let tryFindPieceAt square = game.Board |> Map.tryFind square

  let checkSquaresDistinct =
    let pieceSquare  = Square.parse pieceLocation
    let targetSquare = Square.parse targetLocation
    if pieceSquare = targetSquare then
      Error "no move"
    else
      Ok (pieceSquare, targetSquare)

  let checkTurnToPlay piece =
    if game.Turn <> piece.Color then
      Error $"not yet {piece.Color}'s turn"
    else
      Ok piece

  monad' {
    let! (pieceSquare, targetSquare) = checkSquaresDistinct
    let! movedPiece =
      tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
      >>= checkTurnToPlay
    let board =
      game.Board
      |> Map.remove pieceSquare
      |> Map.add targetSquare movedPiece
    return { game with Board = board }
  }
