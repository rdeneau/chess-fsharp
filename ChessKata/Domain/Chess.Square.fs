namespace ChessKata.Domain

open System
open ChessKata.Common.Helpers
open FSharpPlus

// fsharplint:disable-next-line EnumCasesNames
type File = a = 1 | b = 2 | c = 3 | d = 4 | e = 5 | f = 6 | g = 7 | h = 8 // Column

// fsharplint:disable-next-line EnumCasesNames
type Rank = _1 = 1 | _2 = 2 | _3 = 3 | _4 = 4 | _5 = 5 | _6 = 6 | _7 = 7 | _8 = 8 // Row

type SquareNotation = string // E.g. "a1"

type Square = { Notation: SquareNotation; File: File; Rank: Rank }

[<Measure>] type square

type Side = QueenSide | KingSide
type Direction = Backward | Forward

type Angle =
  | Horizontal of Side
  | Vertical of Direction
  | Diagonal of Side * Direction

type Path = { InsidePath: Square list; Angle: Angle }

module Square =
  let tryParse (notation: SquareNotation) =
    match notation with
    | Regex @"([a-h])([1-8])" [ file; rank ] ->
      Some {
        Notation = notation
        File     = Enum.Parse(typeof<File>, file) :?> File
        Rank     = rank |> int |> enum<Rank>
      }
    | _ -> None

  let parse (notation: SquareNotation) =
    match tryParse notation with
    | Some square -> square
    | None -> failwith "invalid coordinate"

  let create file rank =
    {
      Notation = $"{file}{int rank}"
      File     = file
      Rank     = rank
    }

  /// E.g. "a1" |> offset (1, 1) = "b2"
  let offset (fileDiff, rankDiff) square =
    let file =
      int square.File
      |> (+) fileDiff
      |> enum<File>
    let rank =
      int square.Rank
      |> (+) rankDiff
      |> enum<Rank>
    create file rank

  let private horizontal fileDiff =
    match fileDiff with
    | Negative -> QueenSide
    | Positive -> KingSide
    | _ -> failwith "not horizontal"

  let private vertical rankDiff color =
    match color, rankDiff with
    | White, Positive | Black, Negative -> Forward
    | White, Negative | Black, Positive -> Backward
    | _ -> failwith "not vertical"

  let tryComputePath startSquare endSquare color =
    let fileDiff = int (endSquare.File - startSquare.File)
    let rankDiff = int (endSquare.Rank - startSquare.Rank)

    monad' {
      let! numberOfSquares, angle =
        match fileDiff, rankDiff with
        | 0, 0 -> None
        | 0, r -> Some (r, Vertical (vertical r color))
        | f, 0 -> Some (f, Horizontal (horizontal f))
        | f, r when (abs f) = (abs r) -> Some (f, Diagonal (horizontal f, vertical r color))
        | _ -> None

      let fullPath =
        List.init
          ((abs numberOfSquares) + 1)
          (fun i -> startSquare |> offset (i * sign fileDiff, i * sign rankDiff))

      return { InsidePath = trimList fullPath; Angle = angle }
    }
