namespace ChessKata.Domain

open ChessKata.Common.Helpers
open FSharpPlus

type CheckResult = Check of CheckInfo // TODO | Mate
 and CheckInfo = { Of: Color; By: Square list }

type Move = Forward | Rectilinear of Path | Diagonal of Path | Jump | Other
 and Path = { NumberOfSquares: int; InsidePath: Square list }

type Game = { Board: Map<Square, ColoredPiece>; Turn: Color }

module Game =
  let addPiece pieceSymbol squareNotation game : Game =
    let square = squareNotation |> Square.parse
    let piece = pieceSymbol |> ColoredPiece.parse
    let board = game.Board |> Map.add square piece
    { game with Board = board }

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

    let isForward =
      match file, rank, color, startSquare.Rank with
      | 0,  1, White, _ -> true
      | 0, -1, Black, _ -> true
      | 0,  2, White, Rank._2 -> true
      | 0, -2, Black, Rank._7 -> true
      | _ -> false

    if isForward then
      Forward
    else
      let computePath numberOfSquares =
        let fullPath = List.init (numberOfSquares + 1) (fun i -> startSquare |> Square.add (i * sign file, i * sign rank))
        {
          NumberOfSquares = numberOfSquares
          InsidePath      = trimList fullPath
        }

      match abs file, abs rank with
      | 0, r -> Rectilinear (computePath r)
      | f, 0 -> Rectilinear (computePath f)
      | f, r when f = r -> Diagonal (computePath f)
      | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
      | _ -> Other

  let private movePieceWithoutFinalKingCheck (pieceLocation: SquareNotation) (targetLocation: SquareNotation) (game: Game) : Result<Game, string> =
    let tryFindPieceAt square = game.Board |> Map.tryFind square

    let checkSquaresDistinct =
      let pieceSquare  = Square.parse pieceLocation
      let targetSquare = Square.parse targetLocation
      if pieceSquare = targetSquare then
        Error "no move"
      else
        Ok (pieceSquare, targetSquare)

    let checkTurnToPlay piece =
      if game.Turn <> piece.Color then
        Error $"not {piece.Color}'s turn to play"
      else
        Ok piece

    let checkTarget targetPiece turn =
      match targetPiece with
      | Some { Color = targetColor } when targetColor = turn
        -> Error "move not allowed"
      | _
        -> Ok ()

    let checkPieceMove { Color = turn; Piece = piece } move targetPiece =
      match piece, move with
      | Knight, Jump
      | Queen, Forward
      | Rook, Forward
      | King, Forward
      | King, Diagonal { NumberOfSquares = 1 }
      | King, Rectilinear { NumberOfSquares = 1 }
        -> Ok ()

      | Bishop, Diagonal { InsidePath = insidePath }
      | Queen, Diagonal { InsidePath = insidePath }
      | Queen, Rectilinear { InsidePath = insidePath }
      | Rook, Rectilinear { InsidePath = insidePath }
        ->
          let blockingPieces =
            insidePath
            |> List.choose (fun x -> game.Board |> Map.tryFind x)
          if blockingPieces |> List.isEmpty then
            Ok ()
          else
            Error "move not allowed"

      | Pawn, Forward ->
        match targetPiece with
        | None
          -> Ok ()
        | _
          -> Error "move not allowed"

      | Pawn, Diagonal { NumberOfSquares = 1 } ->
        match targetPiece with
        | Some { Color = targetColor } when targetColor <> turn
          -> Ok () // Capture adversary piece
        | _
          -> Error "move not allowed"

      | _ -> Error "move not allowed"

    monad' {
      let! (pieceSquare, targetSquare) = checkSquaresDistinct

      let! movedPiece =
        tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
        >>= checkTurnToPlay

      let targetPiece = tryFindPieceAt targetSquare
      do! checkTarget targetPiece game.Turn

      let move = computeMove pieceSquare targetSquare movedPiece.Color
      do! checkPieceMove movedPiece move targetPiece

      let promotedPiece =
        match (movedPiece, targetSquare) with
        | { Color = White; Piece = Pawn }, { Rank = Rank._8 } -> ColoredPiece.parse '♕'
        | { Color = Black; Piece = Pawn }, { Rank = Rank._1 } -> ColoredPiece.parse '♛'
        | _ -> movedPiece

      let board =
        game.Board
        |> Map.remove pieceSquare
        |> Map.add targetSquare promotedPiece
      return { game with Board = board } |> toggleTurn
    }

  /// Check if the given player is in check or mate
  let checkPlayer king game : CheckResult option =
    let kingSquare =
      match game |> tryLocatePiece king.Symbol with
      | Some x -> x
      | None -> failwith $"{king.Color} King not found"

    let canMoveToKing adversarySquare : bool =
      let result =
        game
        |> toggleTurn
        |> movePieceWithoutFinalKingCheck adversarySquare.Notation kingSquare.Notation
      match result with
      | Ok _ -> true
      | _ -> false

    let checks =
      game.Board
      |> Map.keys
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
        game |> movePieceWithoutFinalKingCheck pieceLocation targetLocation
      return!
        match newGame |> toggleTurn |> check with
        | None ->
          Ok newGame
        | Some (Check check) ->
          let checkers = check.By |> List.map (fun x -> x.Notation)
          Error $"move to {targetLocation} not allowed: in check by {checkers}"
    }
