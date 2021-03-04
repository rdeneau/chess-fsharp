namespace ChessKata.Domain

open ChessKata.Common.Helpers
open FSharpPlus

type CheckResult = Check of CheckInfo // TODO | Mate
 and CheckInfo = { Of: Color; By: Square list }

type Game = { Board: Map<Square, ColoredPiece>; Turn: Color }

module Game =
  let addPiece pieceSymbol squareNotation game : Game =
    let square = squareNotation |> Square.parse
    let piece = pieceSymbol |> ColoredPiece.parse
    let board = game.Board |> Map.add square piece
    { game with Board = board }

  let private adversarySquares game =
    game.Board
    |> Map.filter (fun _ piece -> piece.Color <> game.Turn)
    |> Map.keys

  let reposition pieceLocation targetLocation game : Game =
    let pieceSquare = Square.parse pieceLocation
    let targetSquare = Square.parse targetLocation
    let piece =
      game.Board
      |> Map.find pieceSquare
    let board =
      game.Board
      |> Map.remove pieceSquare
      |> Map.add targetSquare piece
    { game with Board = board }

  let tryLocatePiece pieceSymbol game : Square option =
    game.Board
    |> Map.tryFindKey (fun _ piece -> piece.Symbol = pieceSymbol)

  let toggleTurn game : Game =
    { game with Turn = game.Turn |> Color.toggle }

  let private computeMove startSquare endSquare color =
    let file = int (endSquare.File - startSquare.File)
    let rank = int (endSquare.Rank - startSquare.Rank)

    match file, rank, color, startSquare with
    | 0,  1, White, _
    | 0, -1, Black, _
    | 0,  2, White, { Rank = Rank._2 }
    | 0, -2, Black, { Rank = Rank._7 } -> Forward

    | -2, 0, White, { Notation = "e1" } -> Castling (White, QueenSide)
    | +2, 0, White, { Notation = "e1" } -> Castling (White, KingSide)
    | -2, 0, Black, { Notation = "e8" } -> Castling (Black, QueenSide)
    | +2, 0, Black, { Notation = "e8" } -> Castling (Black, KingSide)

    | _ ->
      match Square.tryComputePath startSquare endSquare with
      | Some path ->
        match path.Angle with
        | Angle.Horizontal | Angle.Vertical -> Rectilinear path
        | Angle.Diagonal -> Diagonal path
        | _ -> Other
      | None ->
        match abs file, abs rank with
        | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
        | _ -> Other

  type CheckPlayerFn = ColoredPiece -> Game -> CheckResult option

  let rec private movePieceWithoutFinalKingCheck (pieceLocation: SquareNotation)
                                                 (targetLocation: SquareNotation)
                                                 (checkPlayer: CheckPlayerFn)
                                                 (game: Game) : Result<Game, string> =
    let isOccupied square =
      game.Board |> Map.containsKey square

    let tryFindPieceAt square =
      game.Board |> Map.tryFind square

    let verifySquaresDistinct =
      let pieceSquare  = Square.parse pieceLocation
      let targetSquare = Square.parse targetLocation
      if pieceSquare = targetSquare then
        Error "no move"
      else
        Ok (pieceSquare, targetSquare)

    let verifyTurnToPlay piece =
      if game.Turn <> piece.Color then
        Error $"not {piece.Color}'s turn to play"
      else
        Ok piece

    let verifyTarget targetSquare turn =
      let targetPiece = tryFindPieceAt targetSquare
      match targetPiece with
      | Some { Color = targetColor } when targetColor = turn
        -> Error $"move to {targetSquare.Notation} not allowed: square occupied by own piece"
      | _
        -> Ok targetPiece

    let verifyCastlingPathNotUnderAttack path targetSquare =
      let isSquareUnderAttackBy (adversary, square) =
        let attackResult =
          game
          |> toggleTurn
          |> movePieceWithoutFinalKingCheck adversary.Notation square.Notation checkPlayer
        match attackResult with
        | Ok    _ -> true
        | Error _ -> false

      let squareUnderAttack =
        Seq.allPairs (game |> adversarySquares) (path |> Seq.ofList)
        |> Seq.tryFind isSquareUnderAttackBy

      match squareUnderAttack with
      | Some (x, y) ->
        Error $"castling to {targetSquare.Notation} not allowed: king cannot pass through {y.Notation} under attack by {x.Notation}"
      | _ -> Ok ()

    let verifyPathFree path targetSquare =
      let occupiedSquares = path |> List.filter isOccupied
      match occupiedSquares with
      | [] -> Ok ()
      | [x] -> Error $"move to {targetSquare.Notation} not allowed: {x.Notation} occupied"
      | x::_ -> Error $"move to {targetSquare.Notation} not allowed: {x.Notation} occupied"

    let verifyPieceMove coloredPiece move targetPiece targetSquare =
      let { Color = turn; Piece = piece } = coloredPiece
      match piece, move with
      | Knight, Jump
      | Queen, Forward
      | Rook, Forward
      | King, Forward
      | King, Diagonal { InnerSquares = [] }
      | King, Rectilinear { InnerSquares = [] }
        -> Ok ()

      | Queen, Diagonal { InnerSquares = insidePath }
      | Bishop, Diagonal { InnerSquares = insidePath }
      | Rook, Rectilinear { InnerSquares = insidePath }
      | Queen, Rectilinear { InnerSquares = insidePath }
        -> verifyPathFree insidePath targetSquare

      | King, Castling c ->
        let castling = c |> Castling.info
        let verifiedNotInCheck =
          match game |> checkPlayer coloredPiece with
          | Some (Check check) ->
            let checkers = check.By |> List.map (fun x -> x.Notation)
            Error $"castling to {targetSquare.Notation} not allowed: king is currently in check by {checkers}"
          | _ -> Ok ()
        monad' {
          do! verifiedNotInCheck
          do! verifyPathFree castling.InnerSquares targetSquare
            |> Result.mapError (fun err -> err.Replace("move", "castling"))
          do! verifyCastlingPathNotUnderAttack (castling.InnerSquares |> List.take 2) targetSquare
          return!
            tryFindPieceAt castling.RookSquare
            |> Option.filter (fun x -> x.Piece = Rook && x.Color = turn)
            |> Option.map ignore
            |> toResult $"castling to {targetSquare.Notation} not allowed: no rook at {castling.RookSquare.Notation}"
        };

      | Pawn, Forward ->
        // Pawn cannot capture forward -> targetSquare must be free
        match targetPiece with
        | None -> Ok ()
        | _ -> Error $"move to {targetSquare.Notation} not allowed: square occupied"

      | Pawn, Diagonal  { InnerSquares = [] } ->
        // Capture adversary piece?
        match targetPiece with
        | Some { Color = targetColor } when targetColor <> turn
          -> Ok ()
        | _
          -> Error $"move to {targetSquare.Notation} not allowed: no piece to capture by Pawn"

      | _ -> Error $"move to {targetSquare.Notation} not allowed for {piece}"

    let moveRookDuringCastling move board =
      match move with
      | Castling c ->
        let castling = c |> Castling.info
        let rook = board |> Map.find castling.RookSquare
        board
        |> Map.remove castling.RookSquare
        |> Map.add castling.RookTarget rook
      | _ -> board

    monad' {
      let! (pieceSquare, targetSquare) = verifySquaresDistinct

      let! movedPiece =
        tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
        >>= verifyTurnToPlay

      let! targetPiece = verifyTarget targetSquare game.Turn

      let move = computeMove pieceSquare targetSquare movedPiece.Color
      do! verifyPieceMove movedPiece move targetPiece targetSquare

      let promotedPiece =
        match (movedPiece, targetSquare) with
        | { Color = White; Piece = Pawn }, { Rank = Rank._8 } -> ColoredPiece.parse '♕'
        | { Color = Black; Piece = Pawn }, { Rank = Rank._1 } -> ColoredPiece.parse '♛'
        | _ -> movedPiece

      let board =
        game.Board
        |> Map.remove pieceSquare
        |> Map.add targetSquare promotedPiece
        |> moveRookDuringCastling move
      return { game with Board = board } |> toggleTurn
    }

  /// Check if the given player is in check or mate
  let rec private checkPlayer: CheckPlayerFn = fun king game ->
    let kingSquare =
      match king.Piece, game |> tryLocatePiece king.Symbol with
      | King, Some x -> x
      | King, None -> failwith $"{king.Color} King not found"
      | piece, _ -> invalidArg (nameof king) $"Expecting King, received {piece}"

    let canMoveToKing adversarySquare : bool =
      let result =
        game
        |> toggleTurn
        |> movePieceWithoutFinalKingCheck adversarySquare.Notation kingSquare.Notation checkPlayer
      match result with
      | Ok _ -> true
      | _ -> false

    let checks =
      game
      |> adversarySquares
      |> Seq.filter canMoveToKing
      |> List.ofSeq

    match checks with
    | [] -> None
    | xs -> Some (Check { Of = king.Color; By = xs })

  /// Check if the current player is in check or mate
  let check game : CheckResult option =
    let king =
      ['♔';'♚']
      |> List.map ColoredPiece.parse
      |> List.find (fun x -> x.Color = game.Turn)

    game |> checkPlayer king

  let movePiece pieceLocation targetLocation game : Result<Game, string> =
    monad' {
      let! newGame =
        game |> movePieceWithoutFinalKingCheck pieceLocation targetLocation checkPlayer
      return!
        match newGame |> toggleTurn |> check with
        | None ->
          Ok newGame
        | Some (Check check) ->
          let checkers = check.By |> List.map (fun x -> x.Notation)
          Error $"move to {targetLocation} not allowed: in check by {checkers}"
    }
