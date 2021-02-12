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
type PieceSymbol = string
type Game = { Board: Map<Square, Color * Piece>; Turn: Color }

module Piece =
  let parse (piece: PieceSymbol) =
    match piece with
    | "♔" -> White, King
    | "♕" -> White, Queen
    | "♖" -> White, Rook
    | "♗" -> White, Bishop
    | "♘" -> White, Knight
    | "♙" -> White, Pawn
    | "♚" -> Black, King
    | "♛" -> Black, Queen
    | "♜" -> Black, Rook
    | "♝" -> Black, Bishop
    | "♞" -> Black, Knight
    | "♟︎" -> Black, Pawn
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
  let board = game.Board |> Map.add (square |> Square.parse) (piece |> Piece.parse)
  { game with Board = board }

let move (piece: SquareNotation) (target: SquareNotation) (game: Game) : Result<Game, string> =
  let pieceSquare  = Square.parse piece
  let targetSquare = Square.parse target
  if pieceSquare = targetSquare then
    Error "no move"
  elif true then
    Error $"no piece at {piece}"
  else
    Ok game
