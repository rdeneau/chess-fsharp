module ChessKata.Tests.SquareShould

open ChessKata.Domain
open FsUnit
open Swensen.Unquote
open Xunit

let formatSquares squares =
  let res = squares |> List.map (fun x -> x.Notation)
  res.ToString ()

let data =
  let x = TheoryData<SquareNotation, SquareNotation, Color, string, Angle>()
  x.Add ("e1", "f1", White, "[]",           Angle.Horizontal KingSide)
  x.Add ("e1", "g1", White, "[f1]",         Angle.Horizontal KingSide)
  x.Add ("e1", "b1", White, "[d1; c1]",     Angle.Horizontal QueenSide)
  x.Add ("a1", "e1", White, "[b1; c1; d1]", Angle.Horizontal KingSide)
  x.Add ("a1", "a4", White, "[a2; a3]",     Angle.Vertical Forward)
  x.Add ("e5", "e1", White, "[e4; e3; e2]", Angle.Vertical Backward)
  x.Add ("e5", "e1", Black, "[e4; e3; e2]", Angle.Vertical Forward)
  x.Add ("a1", "d4", White, "[b2; c3]", Angle.Diagonal (KingSide, Forward))
  x.Add ("d1", "a4", White, "[c2; b3]", Angle.Diagonal (QueenSide, Forward))
  x.Add ("d1", "a4", Black, "[c2; b3]", Angle.Diagonal (QueenSide, Backward))
  x

[<Theory>]
[<MemberData(nameof data)>]
let ``Compute valid path`` startSquare endSquare color expectedInner expectedAngle =
  let result = Square.tryComputePath (startSquare |> Square.parse) (endSquare |> Square.parse) color
  match result with
  | Some path -> (path.InsidePath |> formatSquares, path.Angle) =! (expectedInner, expectedAngle)
  | None -> failwith "Expected valid path"

[<Theory>]
[<InlineData("a1", "c2")>]
[<InlineData("a1", "b4")>]
[<InlineData("a1", "a1")>]
let ``Compute invalid path`` startSquare endSquare =
  let result = Square.tryComputePath (startSquare |> Square.parse) (endSquare |> Square.parse) White
  result =! None
