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

type Move =
  | Rectilinear of Path
  | Diagonal of Path
  | Castling of Castling
  | Jump
  | Other

[<AutoOpen>]
module Move =
  let (|OneSquare|_|) move =
    match move with
    | Rectilinear { InnerSquares = [] }
    | Diagonal { InnerSquares = [] }
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
        | Horizontal _
        | Vertical _ -> Rectilinear path
        | Oblique _ -> Diagonal path
      | None ->
        match abs file, abs rank with
        | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
        | _ -> Other
