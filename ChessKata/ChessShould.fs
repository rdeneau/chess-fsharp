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
let ``move knight to an empty square reachable by jump`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      7, "➖➖➕➖➕➖➖➖"
      6, "➖➕➖➖➖➕➖➖"
      5, "➖➖➖♘➖➖➖➖"
      4, "➖➕➖➖➖➕➖➖"
      3, "➖➖➕➖➕➖➖➖" ]

[<Fact>]
let ``move knight jumping over piece`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      7, "➖➖➕➖➕➖➖➖"
      6, "➖➕♙♙♙➕➖➖"
      5, "➖➖♙♘♙➖➖➖"
      4, "➖➕♙♙♙➕➖➖"
      3, "➖➖➕➖➕➖➖➖" ]

[<Fact>]
let ``move bishop to an empty square in diagonal`` () =
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
let ``Reject moving bishop when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 4 "➖➖➖♗➖➖➖➖"
    |> addRank 3 "➖➖♙➖➖➖➖➖"
    |> addRank 2 "➖❓➖➖➖♙➖➖"
    |> addRank 1 "➖➖➖➖➖➖❓➖"
  (game |> Game.movePiece "d4" "b2") =! Error "move not allowed"
  (game |> Game.movePiece "d4" "g1") =! Error "move not allowed"

[<Fact>]
let ``Move rook to an empty square rectilinear`` () =
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
let ``Reject moving rook when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 4 "➖➖➖♖➖➖♙❓"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖♙➖➖➖➖"
    |> addRank 1 "➖➖➖❓➖➖➖➖"
  (game |> Game.movePiece "d4" "d1") =! Error "move not allowed"
  (game |> Game.movePiece "d4" "h4") =! Error "move not allowed"

[<Fact>]
let ``Move queen to an empty square reachable`` () =
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
let ``Reject moving queen when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖❓➖❓➖❓➖➖"
    |> addRank 5 "➖➖♙♙♙➖➖➖"
    |> addRank 4 "❓♙➖♕➖➖♙❓"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖♙➖♙➖➖"
    |> addRank 1 "➖➖➖❓➖➖❓➖"

  ["b6";"d6";"f6";"a4";"h4";"d1";"g1"]
  |> List.iter (fun dest -> (game |> Game.movePiece "d4" dest) =! Error "move not allowed")

[<Fact>]
let ``Move queen capturing adversary piece`` () =
  testWhitePieceMove "d5" [
      // Board
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖➖"
      7, "➖➖➖➖➖♟➖➖"
      6, "➖➖➖♟➖➖➖➖"
      5, "➖➖♟♕➖➖➖♟"
      4, "➖➖♘➖♟➖➖➖"
      3, "➖➖➖➖➖➖➖➖"
      2, "➖➖➖♟➖➖➖➖"
      // Reachable squares
      9, "ａｂｃｄｅｆｇｈ"
      8, "➕➖➖➖➖➖➖➖"
      7, "➖➕➖➖➖➕➖➖"
      6, "➖➖➕➕➕➖➖➖"
      5, "➖➖➕♕➕➕➕➕"
      4, "➖➖➖➕➕➖➖➖"
      3, "➖➖➖➕➖➖➖➖"
      2, "➖➖➖➕➖➖➖➖" ]

[<Fact>]
let ``Move king to an empty square adjacent`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖➕➕➕➖➖➖"
      5, "➖➖➕♔➕➖➖➖"
      4, "➖➖➕➕➕➖➖➖" ]

[<Fact>]
let ``Reject moving king when blocked`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖♙♙♙➖➖➖"
      5, "➖➖♙♔♙➖➖➖"
      4, "➖➖♙♙♙➖➖➖" ]

[<Fact>]
let ``Move king capturing adversary piece`` () =
  testWhitePieceMove "d5" [
      // Board
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖♞♛♞➖➖➖"
      5, "➖➖♝♔♝➖➖➖"
      4, "➖➖♜♛♜➖➖➖"
      // Reachable squares
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖➕➕➕➖➖➖"
      5, "➖➖➕♔➕➖➖➖"
      4, "➖➖➕➕➕➖➖➖" ]

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

[<Fact(Skip = "TODO")>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  testWhitePieceMove "c3" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➕➕➕➖➖➖➖" // Can capture in both diagonal or go forward
      4, "➖♟➖♟➖➖➖➖" // Black pawns
      3, "➖➖♙➖➖➖➖➖" ]

[<Fact>]
let ``Reject moving pawn to 1-square diagonal occupied by another own piece`` () =
  testWhitePieceMove "c3" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖♙➕♙➖➖➖➖"
      3, "➖➖♙➖➖➖➖➖" ]

// TODO: Pawn promoted to Queen
