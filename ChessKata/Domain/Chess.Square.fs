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

type Path = { InnerSquares: Square list; Angle : Angle }
and Angle =
  | Horizontal =  0
  | Diagonal   = 45
  | Vertical   = 90

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

  let tryComputePath startSquare endSquare =
    let fileDiff = int (endSquare.File - startSquare.File)
    let rankDiff = int (endSquare.Rank - startSquare.Rank)

    monad' {
      let! (numberOfSquares, angle) =
        match abs fileDiff, abs rankDiff with
//        | 0, 0            -> None
        | 0, r            -> Some (r, Angle.Vertical)
        | f, 0            -> Some (f, Angle.Horizontal)
        | f, r when f = r -> Some (f, Angle.Diagonal)
        | _               -> None

      let fullPath =
        List.init
          (numberOfSquares + 1)
          (fun i -> startSquare |> offset (i * sign fileDiff, i * sign rankDiff))

      return { InnerSquares = trimList fullPath; Angle = angle }
    }
