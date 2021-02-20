module ChessKata.ChessShould

open Chess
open ChessTestHelper
open FsUnit
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Reposition specified piece, removing target piece`` () =
  let game = emptyGame |> Game.addPiece '♙' "e4" |> Game.addPiece '♟' "d5"
  let result = game |> Game.reposition "e4" "d5"
  result =! (emptyGame |> Game.addPiece '♙' "d5")

[<Fact>]
let ``Reject invalid coordinate`` () =
  raises<exn> <@ emptyGame |> Game.movePiece "xx" "a1" @>
  raises<exn> <@ emptyGame |> Game.movePiece "a1" null @>

[<Fact>]
let ``Reject no move`` () =
  let result = emptyGame |> Game.movePiece "c3" "c3"
  result =! Error "no move"

[<Fact>]
let ``Reject invalid piece`` () =
  raises<exn> <@ emptyGame |> Game.addPiece '⛃' "e2" @>

[<Fact>]
let ``Reject no piece at the given coordinate`` () =
  let game = emptyGame |> Game.addPiece '♙' "e2"
  let result = game |> Game.movePiece "c3" "c4"
  result =! Error "no piece at c3"

[<Fact>]
let ``Reject moving a piece of a player for which it's not the turn to play`` () =
  let checkWith turn piece destination expectedError =
    let game = { emptyGame with Turn = turn }
               |> Game.addPiece '♙' "e2" // White
               |> Game.addPiece '♟' "e7" // Black
    let result = game |> Game.movePiece piece destination
    result =! Error expectedError

  checkWith White "e7" "e5" "not Black's turn to play"
  checkWith Black "e2" "e4" "not White's turn to play"

[<Fact>]
let ``Reject moving white pawn to an empty square not reachable forward`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖➕➖➖➖➖➖"
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖➖➖➖➖" ]
  testWhitePieceMove "d3" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖➖➕➖➖➖➖"
      3, "➖➖➖♙➖➖➖➖" ]
  testWhitePieceMove "e4" [
      9, "ａｂｃｄｅｆｇｈ"
      5, "➖➖➖➖➕➖➖➖"
      4, "➖➖➖➖♙➖➖➖" ]

[<Fact>]
let ``Reject moving black pawn to an empty square not reachable forward`` () =
  testBlackPieceMove "c7" [
      9, "ａｂｃｄｅｆｇｈ"
      7, "➖➖♟➖➖➖➖➖"
      6, "➖➖➕➖➖➖➖➖"
      5, "➖➖➕➖➖➖➖➖" ]
  testBlackPieceMove "d6" [
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖➖♟➖➖➖➖"
      5, "➖➖➖➕➖➖➖➖" ]
  testBlackPieceMove "e5" [
      9, "ａｂｃｄｅｆｇｈ"
      5, "➖➖➖➖♟➖➖➖"
      4, "➖➖➖➖➕➖➖➖" ]

[<Fact>]
let ``Reject moving knight to an empty square not reachable by jump`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      7, "➖➖➕➖➕➖➖➖"
      6, "➖➕➖➖➖➕➖➖"
      5, "➖➖➖♘➖➖➖➖"
      4, "➖➕➖➖➖➕➖➖"
      3, "➖➖➕➖➕➖➖➖" ]

[<Fact>]
let ``Reject moving bishop to an empty square not in diagonal`` () =
  testWhitePieceMove "d4" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖➕"
      7, "➕➖➖➖➖➖➕➖"
      6, "➖➕➖➖➖➕➖➖"
      5, "➖➖➕➖➕➖➖➖"
      4, "➖➖➖♗➖➖➖➖"
      3, "➖➖➕➖➕➖➖➖"
      2, "➖➕➖➖➖➕➖➖"
      1, "➕➖➖➖➖➖➕➖" ]

[<Fact>]
let ``Reject moving rook to an empty square not rectilinear`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➕➖➖➖➖"
      7, "➖➖➖➕➖➖➖➖"
      6, "➖➖➖➕➖➖➖➖"
      5, "➕➕➕♖➕➕➕➕"
      4, "➖➖➖➕➖➖➖➖"
      3, "➖➖➖➕➖➖➖➖"
      2, "➖➖➖➕➖➖➖➖"
      1, "➖➖➖➕➖➖➖➖" ]

[<Fact>]
let ``Reject moving queen to an empty square not reachable`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➕➖➖➕➖➖➕➖"
      7, "➖➕➖➕➖➕➖➖"
      6, "➖➖➕➕➕➖➖➖"
      5, "➕➕➕♕➕➕➕➕"
      4, "➖➖➕➕➕➖➖➖"
      3, "➖➕➖➕➖➕➖➖"
      2, "➕➖➖➕➖➖➕➖"
      1, "➖➖➖➕➖➖➖➕" ]

[<Fact>]
let ``Reject moving king to an empty square adjacent`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖➖"
      7, "➖➖➖➖➖➖➖➖"
      6, "➖➖➕➕➕➖➖➖"
      5, "➖➖➕♔➕➖➖➖"
      4, "➖➖➕➕➕➖➖➖"
      3, "➖➖➖➖➖➖➖➖"
      2, "➖➖➖➖➖➖➖➖"
      1, "➖➖➖➖➖➖➖➖" ]

[<Fact>]
let ``Reject moving pawn to a reachable square occupied by another own piece`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖♙➖➖➖➖➖"
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖➖➖➖➖" ]

[<Fact>]
let ``Reject moving pawn to a reachable but not capturable square occupied by adversary piece`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖♟➖➖➖➖➖" // Black
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖➖➖➖➖" ]

// TODO let ``Reject moving pawn to 1-square diagonal occupied by another own piece`` () =

[<Fact(Skip = "TODO")>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  "TODO"

// TODO: Pawn promoted to Queen
// TODO: Pawn blocked ahead cannot move
// TODO: Knight can jump
// TODO: Bishop, Rook, Queen, King blocked before destination cannot move
// TODO: Knight, Bishop, Rook, Queen, King capturing adversary piece
