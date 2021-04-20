namespace ChessKata.Domain

open ChessKata.Common.Helpers
open ColoredPiecePatterns
open FSharpPlus

type CheckResult = Check of CheckInfo // TODO | Mate
 and CheckInfo = { Of: Color; By: Square list }

type MoveLog = { From: SquareNotation; To: SquareNotation; By: ColoredPiece; }

type Game = { Board: Map<Square, ColoredPiece>; Turn: Color; Moves: MoveLog list }

module Game =
  type MoveOk = { Capture: Square option }
  type MoveError = string
  type MoveResult = Result<MoveOk, MoveError>

  module MoveResult =
    let ok () =
      Ok { Capture = None }

    let withoutCapture result =
      result |> Result.map (fun _ -> { Capture = None })

  let addPiece pieceSymbol squareNotation game : Game =
    let square = squareNotation |> Square.parse
    let piece = pieceSymbol |> ColoredPiece.parse
    let board = game.Board |> Map.add square piece
    { game with Board = board }

  let private squaresOfOpposingPieces game =
    game.Board
    |> Map.filter (fun _ piece -> piece.Color <> game.Turn)
    |> Map.keys

  let logMove pieceLocation targetLocation by game =
    let moveToLog = { From = pieceLocation
                      To   = targetLocation
                      By   = by }
    { game with Moves = moveToLog :: game.Moves }

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

  let pieceHasNeverMoved location game =
    game.Moves
    |> List.exists (fun { From = from } -> from = location)
    |> not

  let tryLocatePiece pieceSymbol game : Square option =
    game.Board
    |> Map.tryFindKey (fun _ piece -> piece.Symbol = pieceSymbol)

  let toggleTurn game : Game =
    { game with Turn = game.Turn |> Color.toggle }

  type CheckPlayerFn = ColoredPiece -> Game -> CheckResult option

  let rec private movePieceWithoutFinalKingCheck (pieceLocation: SquareNotation)
                                                 (targetLocation: SquareNotation)
                                                 (checkPlayer: CheckPlayerFn)
                                                 (game: Game) : Result<Game, string> =
    let isOccupied square =
      game.Board |> Map.containsKey square

    let notAllowed (move: string) (reason: string) =
      let message = $"{move} {pieceLocation}-{targetLocation} not allowed"
      if System.String.IsNullOrWhiteSpace(reason)
      then message
      else $"{message}: {reason}"

    let castlingNotAllowed = "castling" |> notAllowed
    let moveNotAllowed = "move" |> notAllowed

    let tryFindPieceAt square =
      game.Board |> Map.tryFind square

    let verifyPathFree path =
      let occupiedSquares = path |> List.filter isOccupied
      match occupiedSquares with
      | []   -> Ok ()
      | x::_ -> Error <| moveNotAllowed $"{x.Notation} occupied"

    let verifySquaresDistinct =
      let pieceSquare  = Square.parse pieceLocation
      let targetSquare = Square.parse targetLocation
      match pieceSquare = targetSquare with
      | true  -> Error "no move"
      | false -> Ok (pieceSquare, targetSquare)

    let verifyTarget targetSquare player =
      let targetPiece = tryFindPieceAt targetSquare
      match targetPiece with
      | Some (OwnPieceOf player) -> Error <| moveNotAllowed "square occupied by own piece"
      | _ -> Ok targetPiece

    let verifyTurnToPlay piece =
      if game.Turn <> piece.Color
      then Error $"not {piece.Color}'s turn to play"
      else Ok piece

    let verifyCastlingPathNotUnderAttack path =
      let isSquareUnderAttackBy (adversary, square) =
        let attackResult =
          game
          |> toggleTurn
          |> movePieceWithoutFinalKingCheck adversary.Notation square.Notation checkPlayer
        match attackResult with
        | Ok    _ -> true
        | Error _ -> false

      let squareUnderAttack =
        Seq.allPairs (game |> squaresOfOpposingPieces) (path |> Seq.ofList)
        |> Seq.tryFind isSquareUnderAttackBy

      match squareUnderAttack with
      | Some (x, y) ->
        Error <| castlingNotAllowed $"king cannot pass through {y.Notation} under attack by {x.Notation}"
      | _ -> Ok ()

    let verifyCastling coloredPiece castling =
      let verifyPieceHasNeverMoved name location =
        match game |> pieceHasNeverMoved location with
        | false -> Error <| castlingNotAllowed $"{name} has previously moved"
        | true  -> Ok ()

      let verifyNotInCheck () =
        match game |> checkPlayer coloredPiece with
        | Some (Check check) ->
          let checkers = check.By |> List.map (fun x -> x.Notation)
          Error <| castlingNotAllowed $"king is currently in check by {checkers}"
        | _ -> Ok ()

      monad' {
        do! verifyNotInCheck ()
        do! verifyPieceHasNeverMoved "king" pieceLocation
        do! verifyPieceHasNeverMoved "rook" castling.RookSquare.Notation
        do! verifyPathFree castling.InnerSquares
          |> Result.mapError (fun err -> err.Replace("move", "castling"))
        do! verifyCastlingPathNotUnderAttack (castling.InnerSquares |> List.truncate 2)
        return!
          tryFindPieceAt castling.RookSquare
          |> Option.filter (fun x -> x.Piece = Rook && x.Color = coloredPiece.Color)
          |> Option.map ignore
          |> toResult (castlingNotAllowed $"no rook at {castling.RookSquare.Notation}")
      }

    let verifyPawnCaptureEnPassant player : MoveResult =
      let isOnFifthRank =
        let { Rank = rank } = Square.parse pieceLocation
        match player, rank with
        | White, Rank._5
        | Black, Rank._3 -> true
        | _              -> false

      let hasPreviousMove, target, fromTwoSquareAhead, upToAdjacentSquare =
        match game.Moves with
        | { From = from; To = dest; By = { Color = enemy; Piece = Pawn } } :: _ ->
          let isAdjacent = Square.areAdjacent pieceLocation dest
          let target = Square.parse dest
          let previousMove = computeMove (Square.parse from) target enemy
          let twoSquareBeforeDest =
            match previousMove with
            | Vertical _ & ForwardBy 2<square> -> true
            | _ -> false
          true, Some target, twoSquareBeforeDest, isAdjacent
        | _ ->
          false, None, false, false

      match isOnFifthRank && hasPreviousMove && fromTwoSquareAhead && upToAdjacentSquare with
      | true -> Ok { Capture = target }
      | _ -> Error <| moveNotAllowed "no piece in diagonal to capture by pawn"

    let verifyPawnMoveDiagonally targetPiece player : MoveResult =
      match targetPiece with
      | Some (OpposingPieceOf player) -> MoveResult.ok()
      | _ -> verifyPawnCaptureEnPassant player

    let verifyPawnMoveForward insidePath nbSquares targetPiece turn =
      monad' {
        let hasNeverMoved =
          let { Rank = rank } = Square.parse pieceLocation
          match turn, rank with
          | White, Rank._2
          | Black, Rank._7 -> true
          | _              -> false

        do! match nbSquares, hasNeverMoved with
            | 1<square>, _     -> Ok ()
            | 2<square>, true  -> verifyPathFree insidePath
            | 2<square>, false -> Error <| moveNotAllowed "pawn has previously moved"
            | _                -> Error <| moveNotAllowed "pawn cannot move forward on more than 2 squares"

        do! match targetPiece with
            | None   -> Ok ()
            | Some _ -> Error <| moveNotAllowed "forward square occupied and pawn cannot capture forward"
      }

    let verifyPieceMove coloredPiece move targetPiece : MoveResult =
      let { Color = turn; Piece = piece } = coloredPiece
      match piece, move with
      | Knight, Jump
      | King, OneSquare ->
        MoveResult.ok()

      | King, Castling castling ->
        castling
        |> Castling.info
        |> verifyCastling coloredPiece
        |> MoveResult.withoutCapture

      | Bishop, Diagonal  { InsidePath = insidePath }
      | Queen, Diagonal   { InsidePath = insidePath }
      | Queen, Horizontal { InsidePath = insidePath }
      | Queen, Vertical   { InsidePath = insidePath }
      | Rook, Horizontal  { InsidePath = insidePath }
      | Rook, Vertical    { InsidePath = insidePath } ->
        verifyPathFree insidePath
        |> MoveResult.withoutCapture

      | Pawn, Diagonal _ & ForwardBy 1<square> ->
        verifyPawnMoveDiagonally targetPiece turn

      | Pawn, Vertical { InsidePath = insidePath } & ForwardBy nbSquares ->
        verifyPawnMoveForward insidePath nbSquares targetPiece turn
        |> MoveResult.withoutCapture

      | _ ->
        Error <| moveNotAllowed $"for {piece}"

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
      let! pieceSquare, targetSquare = verifySquaresDistinct

      let! movedPiece =
        tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
        >>= verifyTurnToPlay

      let! targetPiece = verifyTarget targetSquare game.Turn

      let move = computeMove pieceSquare targetSquare movedPiece.Color
      let! success = verifyPieceMove movedPiece move targetPiece

      let isNotCapturedSquare k _ =
        match success.Capture with
        | Some x -> x <> k
        | None -> true

      let promotedPiece =
        match (movedPiece, targetSquare) with
        | { Color = White; Piece = Pawn }, { Rank = Rank._8 } -> ColoredPiece.parse '♕'
        | { Color = Black; Piece = Pawn }, { Rank = Rank._1 } -> ColoredPiece.parse '♛'
        | _ -> movedPiece

      let board =
        game.Board
        |> Map.remove pieceSquare
        |> Map.filter isNotCapturedSquare
        |> Map.add targetSquare promotedPiece
        |> moveRookDuringCastling move

      return
        { game with Board = board }
        |> logMove pieceLocation targetLocation movedPiece
        |> toggleTurn
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
      |> squaresOfOpposingPieces
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
