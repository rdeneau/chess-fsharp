module Chess

open FSharpPlus
open System

open Helpers

type File = a = 1 | b = 2 | c = 3 | d = 4 | e = 5 | f = 6 | g = 7 | h = 8 // Column
type Rank = _1 = 1 | _2 = 2 | _3 = 3 | _4 = 4 | _5 = 5 | _6 = 6 | _7 = 7 | _8 = 8 // Row
type SquareNotation = string // E.g. "a1"
type Square = { Notation: SquareNotation; File: File; Rank: Rank }

let allSquareNotations: SquareNotation list =
  [for file in enumValues<File> do
   for rank in enumValues<Rank> do
     yield $"{file}{int rank}" ]

type PieceSymbol = char
type Piece = King | Queen | Rook | Bishop | Knight | Pawn
type Color = Black | White
type Move = Forward | Rectilinear of int | Diagonal of int | Jump | Other

type ColoredPiece = { Color: Color; Piece: Piece }
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

let computeMove startSquare endSquare color =
  let file = int (endSquare.File - startSquare.File)
  let rank = int (endSquare.Rank - startSquare.Rank)

  let isForward =
    match file, rank, color, startSquare.Rank with
    | 0,  1, White, _ -> true
    | 0, -1, Black, _ -> true
    | 0,  2, White, Rank._2 -> true
    | 0, -2, Black, Rank._7 -> true
    | _ -> false

  if isForward then
    Forward
  else
    match abs file, abs rank with
    | 0, r -> Rectilinear r
    | f, 0 -> Rectilinear f
    | f, r when f = r -> Diagonal f
    | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
    | _ -> Other

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
      Error $"not {piece.Color}'s turn to play"
    else
      Ok piece

  let checkPieceMove { Piece = piece } move =
    match piece, move with
    | Pawn, Forward
    | Knight, Jump
    | Bishop, Diagonal _

    | Rook, Forward
    | Rook, Rectilinear _

    | Queen, Forward
    | Queen, Diagonal _
    | Queen, Rectilinear _

    | King, Forward
    | King, Diagonal 1
    | King, Rectilinear 1
      -> Ok ()
    | _
      -> Error "move not allowed"

  monad' {
    let! (pieceSquare, targetSquare) = checkSquaresDistinct
    let! movedPiece =
      tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
      >>= checkTurnToPlay

    let move = computeMove pieceSquare targetSquare movedPiece.Color
    do! checkPieceMove movedPiece move

    let board =
      game.Board
      |> Map.remove pieceSquare
      |> Map.add targetSquare movedPiece
    return { game with Board = board }
  }
