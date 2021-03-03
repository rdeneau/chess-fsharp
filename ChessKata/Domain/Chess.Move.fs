namespace ChessKata.Domain

open FSharpPlus

type Castling = Color * CastlingSide
 and CastlingSide = QueenSide | KingSide

module Castling =
  let info castling =
    let makeInfo rookStart rookEnd innerSquares : {| InnerSquares: Square list; RookSquare: Square; RookTarget: Square |} =
      {| InnerSquares = innerSquares |> List.map Square.parse
         RookSquare   = rookStart |> Square.parse
         RookTarget   = rookEnd |> Square.parse |}
    match castling with
    | White, QueenSide -> makeInfo "a1" "d1" ["d1"; "c1"; "b1"]
    | Black, QueenSide -> makeInfo "a8" "d8" ["d8"; "c8"; "b8"]
    | White, KingSide  -> makeInfo "h1" "f1" ["f1"; "g1"]
    | Black, KingSide  -> makeInfo "h8" "f8" ["f8"; "g8"]

type Move =
  | Forward
  | Rectilinear of Path
  | Diagonal of Path
  | Castling of Castling
  | Jump
  | Other
