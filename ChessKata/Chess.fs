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
type Move = Forward | Rectilinear | Diagonal of int | Jump | Other

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
  match int (endSquare.File - startSquare.File),
        int (endSquare.Rank - startSquare.Rank) with
  | 0,  1 when color = White -> Forward
  | 0, -1 when color = Black -> Forward
  | 0,  2 when color = White && startSquare.Rank = Rank._2 -> Forward
  | 0, -2 when color = Black && startSquare.Rank = Rank._7 -> Forward
  | 0, _ -> Rectilinear
  | _, 0 -> Rectilinear
  | file, rank when abs file = abs rank -> Diagonal (abs file)
  | file, rank when abs file + abs rank = 3 && abs (file - rank) = 1 -> Jump
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
      -> Ok ()
    // TODO: other pieces
    | _ -> Error "move not allowed"

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
