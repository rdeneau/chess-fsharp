namespace ChessKata.Domain

open FSharpPlus

type Castling = Color * Side

type CastlingInfo =
  { InnerSquares: Square list
    RookSquare  : Square
    RookTarget  : Square }

module Castling =
  let info castling =
    let makeInfo rookStart rookEnd innerSquares =
      { InnerSquares = innerSquares |> List.map Square.parse
        RookSquare   = rookStart |> Square.parse
        RookTarget   = rookEnd |> Square.parse }
    match castling with
    | White, QueenSide -> makeInfo "a1" "d1" ["d1"; "c1"; "b1"]
    | Black, QueenSide -> makeInfo "a8" "d8" ["d8"; "c8"; "b8"]
    | White, KingSide  -> makeInfo "h1" "f1" ["f1"; "g1"]
    | Black, KingSide  -> makeInfo "h8" "f8" ["f8"; "g8"]

type HorizontalPath = { InsidePath: Square list }
type VerticalPath = { InsidePath: Square list; Direction: Direction }
type DiagonalPath = { InsidePath: Square list; Direction: Direction }

type Move =
  | Horizontal of HorizontalPath
  | Vertical of VerticalPath
  | Diagonal of DiagonalPath
  | Castling of Castling
  | Jump
  | Other

[<AutoOpen>]
module Move =
  let (|ForwardBy|_|) move =
    match move with
    | Vertical { Direction = Forward; InsidePath = innerSquares }
    | Diagonal { Direction = Forward; InsidePath = innerSquares } ->
      List.length innerSquares
      |> (+) 1
      |> (*) 1<square>
      |> Some
    | _ -> None

  let (|OneSquare|_|) move =
    match move with
    | Horizontal { InsidePath = [] }
    | Vertical   { InsidePath = [] }
    | Diagonal   { InsidePath = [] } ->
      Some OneSquare
    | _ -> None

  let computeMove startSquare endSquare color =
    let file = int (endSquare.File - startSquare.File)
    let rank = int (endSquare.Rank - startSquare.Rank)

    let { Notation = startSquareNotation } = startSquare
    match color, startSquareNotation, file, rank with
    | White, "e1", -2, 0 -> Castling (White, QueenSide)
    | White, "e1", +2, 0 -> Castling (White, KingSide)
    | Black, "e8", -2, 0 -> Castling (Black, QueenSide)
    | Black, "e8", +2, 0 -> Castling (Black, KingSide)
    | _ ->
      match Square.tryComputePath startSquare endSquare color with
      | Some path ->
        match path.Angle with
        | Angle.Horizontal _ -> Horizontal { InsidePath = path.InsidePath }
        | Angle.Vertical direction -> Vertical { InsidePath = path.InsidePath; Direction = direction }
        | Angle.Diagonal (_, direction) -> Diagonal { InsidePath = path.InsidePath; Direction = direction }
      | None ->
        match abs file, abs rank with
        | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
        | _ -> Other
