module ChessKata.Tests.GameShould

open ChessKata.Domain
open ChessKata.Tests.ChessHelpers
open FSharpPlus
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
let ``Move white pawn forward`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖♚"
      4, "➖➖➕➖➖➖➖➖"
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖♔➖➖➖" ]
  testWhitePieceMove "d3" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖♚"
      4, "➖➖➖➕➖➖➖➖"
      3, "➖➖➖♙♔➖➖➖" ]
  testWhitePieceMove "e4" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖➖➖➖♚"
      5, "➖➖➖➖➕➖➖➖"
      4, "➖➖➖♔♙➖➖➖" ]

[<Fact>]
let ``Move black pawn forward`` () =
  testBlackPieceMove "c7" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➖"
      7, "➖➖♟➖➖➖➖➖"
      6, "➖➖➕➖➖➖➖➖"
      5, "➖➖➕➖➖➖➖♔" ]
  testBlackPieceMove "d6" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➖"
      6, "➖➖➖♟➖➖➖➖"
      5, "➖➖➖➕➖➖➖♔" ]
  testBlackPieceMove "e5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➖"
      5, "➖➖➖➖♟➖➖➖"
      4, "➖➖➖➖➕➖➖♔" ]

[<Fact>]
let ``Move knight to an empty square reachable by jump`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➖"
      7, "➖➖➕➖➕➖➖➖"
      6, "➖➕➖➖➖➕➖➖"
      5, "➖➖➖♘➖➖➖➖"
      4, "➖➕➖➖➖➕➖➖"
      3, "➖➖➕➖➕➖➖➖"
      2, "➖➖➖➖➖➖➖➖"
      1, "➖➖➖➖♔➖➖➖" ]

[<Fact>]
let ``Move knight jumping over piece`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➖"
      7, "➖➖➕➖➕➖➖➖"
      6, "➖➕♙♙♙➕➖➖"
      5, "➖➖♙♘♙➖➖➖"
      4, "➖➕♙♙♙➕➖➖"
      3, "➖➖➕➖➕➖➖➖"
      2, "➖➖➖➖➖➖➖➖"
      1, "➖➖➖➖♔➖➖➖" ]

[<Fact>]
let ``Move bishop to an empty square in diagonal`` () =
  testWhitePieceMove "d4" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➖♚➖➖➕"
      7, "➕➖➖➖➖➖➕➖"
      6, "➖➕➖➖➖➕➖➖"
      5, "➖➖➕➖➕➖➖➖"
      4, "➖➖➖♗➖➖➖➖"
      3, "➖➖➕➖➕➖➖➖"
      2, "➖➕➖➖➖➕➖➖"
      1, "➕➖➖➖♔➖➕➖" ]

[<Fact>]
let ``Reject moving bishop when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖➖➖➖➖♚➖"
    |> addRank 4 "➖➖➖♗➖➖➖➖"
    |> addRank 3 "➖➖♙➖➖➖➖➖"
    |> addRank 2 "➖❓➖➖➖♙➖➖"
    |> addRank 1 "➖➖➖♔➖➖❓➖"
  (game |> Game.movePiece "d4" "b2") =! Error "move to b2 not allowed: c3 occupied"
  (game |> Game.movePiece "d4" "g1") =! Error "move to g1 not allowed: f2 occupied"

[<Fact>]
let ``Move rook to an empty square rectilinear`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖➕➖♚➖➖"
      7, "➖➖➖➕➖➖➖➖"
      6, "➖➖➖➕➖➖➖➖"
      5, "➕➕➕♖➕➕➕➕"
      4, "➖➖➖➕➖➖➖➖"
      3, "➖➖➖➕➖➖➖➖"
      2, "➖➖➖➕➖➖➖➖"
      1, "➖➖➖➕➖♔➖➖" ]

[<Fact>]
let ``Reject moving rook when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖➖➖➖➖♚➖"
    |> addRank 4 "➖➖➖♖➖➖♙❓"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖♙➖➖➖➖"
    |> addRank 1 "➖♔➖❓➖➖➖➖"
  (game |> Game.movePiece "d4" "d1") =! Error "move to d1 not allowed: d2 occupied"
  (game |> Game.movePiece "d4" "h4") =! Error "move to h4 not allowed: g4 occupied"

[<Fact>]
let ``Move queen to an empty square reachable`` () =
  testWhitePieceMove "d5" [
      9, "ａｂｃｄｅｆｇｈ"
      8, "➕➖➖➕➖♚➕➖"
      7, "➖➕➖➕➖➕➖➖"
      6, "➖➖➕➕➕➖➖➖"
      5, "➕➕➕♕➕➕➕➕"
      4, "➖➖➕➕➕➖➖➖"
      3, "➖➕➖➕➖➕➖➖"
      2, "➕➖➖➕➖➖➕➖"
      1, "➖➖➖➕➖♔➖➕" ]

[<Fact>]
let ``Reject moving queen when blocked on the way`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖❓➖❓➖❓➖♚"
    |> addRank 5 "➖➖♙♙♙➖➖➖"
    |> addRank 4 "❓♙➖♕➖➖♙❓"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖♙➖♙➖➖"
    |> addRank 1 "➖♔➖❓➖➖❓➖"

  [ "b6", "c5"
    "d6", "d5"
    "f6", "e5"
    "a4", "b4"
    "h4", "g4"
    "d1", "d2"
    "g1", "f2" ]
  |> List.iter (fun (dest, blockedBy) ->
    (game |> Game.movePiece "d4" dest) =! Error $"move to {dest} not allowed: {blockedBy} occupied" )

[<Fact>]
let ``Move queen capturing adversary piece`` () =
  testWhitePieceMove "d5" [
      // Board
      9, "ａｂｃｄｅｆｇｈ"
      8, "➖➖➖♚➖➖➖➖"
      7, "➖➖➖➖➖♟➖➖"
      6, "➖➖➖♟➖➖➖➖"
      5, "➖➖♟♕➖➖➖♟"
      4, "➖➖♘➖♟➖➖➖"
      3, "➖➖➖➖➖➖➖➖"
      2, "➖➖➖♟➖♔➖➖"
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
      6, "➖➖➕➕➕➖➖♚"
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
      6, "➖➖➖♞➖➖➖♚"
      5, "➖➖♞♔♞➖➖➖"
      4, "➖➖➖♞➖➖➖➖"
      // Reachable squares
      9, "ａｂｃｄｅｆｇｈ"
      6, "➖➖➖➕➖➖➖➖"
      5, "➖➖➕♔➕➖➖➖"
      4, "➖➖➖➕➖➖➖➖" ]

[<Fact>]
let ``Reject moving pawn to a reachable square occupied by another own piece`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖♙➖➖➖➖♚"
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖➖➖➖♔" ]

[<Fact>]
let ``Reject moving pawn to a reachable but not capturable square occupied by adversary piece`` () =
  testWhitePieceMove "c2" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➖♟➖➖➖➖♚" // Black
      3, "➖➖➕➖➖➖➖➖"
      2, "➖➖♙➖➖➖➖♔" ]

[<Fact>]
let ``Move pawn to 1-square diagonal to capture an adversary piece`` () =
  testWhitePieceMove "c3" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖➕➕➕➖♚➖➖" // Can capture in both diagonal or go forward
      4, "➖♟➖♟➖➖➖➖" // Black pawns
      3, "➖➖♙➖➖➖➖♔" ]

[<Fact>]
let ``Reject moving pawn to 1-square diagonal occupied by another own piece`` () =
  testWhitePieceMove "c3" [
      9, "ａｂｃｄｅｆｇｈ"
      4, "➖♙➕♙➖♚➖➖"
      3, "➖➖♙➖➖➖➖♔" ]

[<Fact>]
let ``Promote black pawn moved to 1th rank`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖➖➖➖♚➖➖"
    |> addRank 2 "➖♟➖➖➖➖➖➖"
    |> addRank 1 "➖➖➖➖➖♔➖➖"
    |> Game.toggleTurn

  let expected =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖➖➖➖♚➖➖"
    |> addRank 2 "➖➖➖➖➖➖➖➖"
    |> addRank 1 "➖♛➖➖➖♔➖➖"

  (game |> Game.movePiece "b2" "b1") =! Ok expected

[<Fact>]
let ``Promote white pawn moved to 8th rank`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖➖➖➖♚➖➖"
    |> addRank 7 "♙➖➖➖➖➖➖➖"
    |> addRank 1 "➖➖➖➖➖♔➖➖"

  let expected =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "♕➖➖➖➖♚➖➖"
    |> addRank 2 "➖➖➖➖➖➖➖➖"
    |> addRank 1 "➖➖➖➖➖♔➖➖"
    |> Game.toggleTurn

  (game |> Game.movePiece "a7" "a8") =! Ok expected

[<Fact>]
let ``Indicate no checks`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖➖♚➖➖➖➖➖"
    |> addRank 5 "➖➖➖➖➖➖➖➖"
    |> addRank 4 "➖➖➖➖♙➖➖➖"
    |> addRank 3 "➖➖➖➖➖♗➖➖"
    |> addRank 2 "➖➖♙➖➖➖➖➖"
    |> addRank 1 "➖➖♖➖♔➖➖➖"
    |> Game.toggleTurn
  let result = game |> Game.check
  result =! None

[<Fact>]
let ``Indicate black in check once`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖➖♚➖➖➖➖➖"
    |> addRank 5 "➖➖➖➖➖➖➖➖"
    |> addRank 4 "➖➖➖➖♙➖➖➖"
    |> addRank 3 "➖➖➖➖➖♗➖➖"
    |> addRank 2 "➖➖➖➖➖➖➖➖"
    |> addRank 1 "➖➖♖➖♔➖➖➖"
    |> Game.toggleTurn
  let result = game |> Game.check
  result =! Some (Check { Of = Black; By = [square "c1"] })

[<Fact>]
let ``Indicate black in check twice`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖➖♚➖➖➖➖➖"
    |> addRank 5 "➖➖➖➖➖➖➖➖"
    |> addRank 4 "➖➖➖➖➖➖➖➖"
    |> addRank 3 "➖➖➖➖➖♗➖➖"
    |> addRank 2 "➖➖➖➖➖➖➖➖"
    |> addRank 1 "➖➖♖➖♔➖➖➖"
    |> Game.toggleTurn
  let result = game |> Game.check
  result =! Some (Check { Of = Black; By = [square "c1"; square "f3"] })

[<Fact>]
let ``Indicate white in check`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖➖➖♚♜➖➖➖"
    |> addRank 5 "➖➖➖➖➖➖➖➖"
    |> addRank 4 "➖➖➖➖➖➖➖➖"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖➖➖➖➖➖"
    |> addRank 1 "➖➖➖➖♔➖➖➖"
  let result = game |> Game.check
  result =! Some (Check { Of = White; By = [square "e6"] })

[<Fact>]
let ``Reject move ending up in own check`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 6 "➖➖➖♚♛♜➖➖"
    |> addRank 5 "➖➖➖➖➖➖➖➖"
    |> addRank 4 "➖➖➖➖➖➖➖➖"
    |> addRank 3 "➖➖➖➖➖➖➖➖"
    |> addRank 2 "➖➖➖➖➖♗➖➖"
    |> addRank 1 "➖➖➖➖➖♔➖➖"
  (game |> Game.movePiece "f1" "e1") =! Error "move to e1 not allowed: in check by [e6]" // King puts himself in check
  (game |> Game.movePiece "f2" "g3") =! Error "move to g3 not allowed: in check by [f6]" // Bishop move puts its king in check

[<Fact>]
let ``Reject castling given missing rook`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "❌➖❓➖♚➖❓❌"
    |> addRank 1 "❌➖❓➖♔➖❓❌"
  (game |> Game.movePiece "e1" "c1") =! Error "castling to c1 not allowed: no rook at a1"
  (game |> Game.movePiece "e1" "g1") =! Error "castling to g1 not allowed: no rook at h1"

  let game = game |> Game.toggleTurn
  (game |> Game.movePiece "e8" "c8") =! Error "castling to c8 not allowed: no rook at a8"
  (game |> Game.movePiece "e8" "g8") =! Error "castling to g8 not allowed: no rook at h8"

[<Fact>]
let ``Reject castling given path blocked`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "♜♞❓♛♚➖♞♜"
    |> addRank 1 "♖♘❓➖♔♗❓♖"
  (game |> Game.movePiece "e1" "c1") =! Error "castling to c1 not allowed: b1 occupied"
  (game |> Game.movePiece "e1" "g1") =! Error "castling to g1 not allowed: f1 occupied"

  let game = game |> Game.toggleTurn
  (game |> Game.movePiece "e8" "c8") =! Error "castling to c8 not allowed: d8 occupied" // Indicate first blockage (d8, not b8)
  (game |> Game.movePiece "e8" "g8") =! Error "move to g8 not allowed: square occupied by own piece"

[<Fact>]
let ``Perform castling move`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "♜➖❓➖♚➖❓♜"
    |> addRank 1 "♖➖❓➖♔➖❓♖"
  (game |> Game.movePiece "e1" "c1") =! Ok (game |> setRank 1 "➖➖♔♖➖➖➖♖" |> Game.toggleTurn)
  (game |> Game.movePiece "e1" "g1") =! Ok (game |> setRank 1 "♖➖➖➖➖♖♔➖" |> Game.toggleTurn)

  let game = game |> Game.toggleTurn
  (game |> Game.movePiece "e8" "c8") =! Ok (game |> setRank 8 "➖➖♚♜➖➖➖♜" |> Game.toggleTurn)
  (game |> Game.movePiece "e8" "g8") =! Ok (game |> setRank 8 "♜➖➖➖➖♜♚➖" |> Game.toggleTurn)

[<Fact>]
let ``Reject castling given king is currently in check`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖♚➖♜➖➖➖"
    |> addRank 1 "♖➖➖➖♔➖➖♖"
  game |> Game.movePiece "e1" "c1"
    =! Error "castling to c1 not allowed: king is currently in check by [e8]"

[<Fact>]
let ``Reject castling given king passes through a square under attack`` () =
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖➖♚♜➖➖➖➖"
    |> addRank 1 "♖➖➖➖♔➖➖♖"
  game |> Game.movePiece "e1" "c1"
    =! Error "castling to c1 not allowed: king cannot pass through d1 under attack by d8"

  // Ok if b1 under attack: not in the king path
  let game =
    emptyGame
    |> addRank 9 "ａｂｃｄｅｆｇｈ"
    |> addRank 8 "➖♜➖♚➖➖➖➖"
    |> addRank 1 "♖➖➖➖♔➖➖♖"
  game
  |> Game.movePiece "e1" "c1"
  |> Result.map ignore
  =! Ok()

[<Fact(Skip = "TODO")>] // TODO
let ``Reject castling given rook has previously moved`` () =
  monad' {
    // Move rook forth...
    let! game1 =
      emptyGame
      |> addRank 9 "ａｂｃｄｅｆｇｈ"
      |> addRank 8 "➖➖♚➖➖➖➖➖"
      |> addRank 1 "♖➖➖➖♔➖➖♖"
      |> Game.movePiece "a1" "a2"
    // ... and back
    let! game2 =
      game1
      |> Game.toggleTurn
      |> Game.movePiece "a2" "a1"
    // Attempt a castling
    return!
      game2
      |> Game.toggleTurn
      |> Game.movePiece "a1" "c1"
  } =! Error "castling to c1 not allowed: rook has previously moved"

// TODO ``Reject castling given king has previously moved``
