module ChessKata.Tests.SquareShould

open ChessKata.Domain
open FsUnit
open Swensen.Unquote
open Xunit

let formatSquares squares =
  let res = squares |> List.map (fun x -> x.Notation)
  res.ToString ()

[<Theory>]
[<InlineData("e1", "f1", "[]", Angle.Horizontal)>]
[<InlineData("e1", "g1", "[f1]", Angle.Horizontal)>]
[<InlineData("e1", "b1", "[d1; c1]", Angle.Horizontal)>]
[<InlineData("a1", "e1", "[b1; c1; d1]", Angle.Horizontal)>]
[<InlineData("a1", "a4", "[a2; a3]", Angle.Vertical)>]
[<InlineData("e5", "e1", "[e4; e3; e2]", Angle.Vertical)>]
[<InlineData("a1", "d4", "[b2; c3]", Angle.Diagonal)>]
let ``Compute valid path`` startSquare endSquare expectedInner expectedAngle =
  let result = Square.tryComputePath (startSquare |> Square.parse) (endSquare |> Square.parse)
  match result with
  | Some path -> (path.InnerSquares |> formatSquares, path.Angle) =! (expectedInner, expectedAngle)
  | None -> failwith "Expected valid path"

[<Theory>]
[<InlineData("a1", "c2")>]
[<InlineData("a1", "b4")>]
[<InlineData("a1", "a1")>]
let ``Compute invalid path`` startSquare endSquare =
  let result = Square.tryComputePath (startSquare |> Square.parse) (endSquare |> Square.parse)
  result =! None
