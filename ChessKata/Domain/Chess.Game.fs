namespace ChessKata.Domain

open ChessKata.Common.Helpers
open FSharpPlus

type CheckResult = Check of CheckInfo // TODO | Mate
 and CheckInfo = { Of: Color; By: Square list }

type Move =
  | Forward
  | Rectilinear of Path
  | Diagonal of Path
  | Castling of {| InsidePath: Square list; RookSquare: Square |}
  | Jump
  | Other
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

    let computePath numberOfSquares =
      let fullPath = List.init (numberOfSquares + 1) (fun i -> startSquare |> Square.offset (i * sign file, i * sign rank))
      {
        NumberOfSquares = numberOfSquares
        InsidePath      = trimList fullPath
      }

    let baseMove =
      match abs file, abs rank with
      | 0, r -> Rectilinear (computePath r)
      | f, 0 -> Rectilinear (computePath f)
      | f, r when f = r -> Diagonal (computePath f)
      | f, r when f + r = 3 && abs(f - r) = 1 -> Jump
      | _ -> Other

    let castling offsetFile rookSquare =
      let insidePath =
        List.init
          (abs offsetFile)
          (fun i -> startSquare |> Square.offset (offsetFile / (i + 1), 0))
      Castling {| InsidePath = insidePath; RookSquare = rookSquare |> Square.parse |}

    match file, rank, color, startSquare with
    | 0,  1, White, _
    | 0, -1, Black, _
    | 0,  2, White, { Rank = Rank._2 }
    | 0, -2, Black, { Rank = Rank._7 }
      -> Forward

    | -2, 0, White, { Notation = "e1" } -> castling -3 "a1"
    | -2, 0, Black, { Notation = "e8" } -> castling -3 "a8"
    | +2, 0, White, { Notation = "e1" } -> castling +2 "h1"
    | +2, 0, Black, { Notation = "e8" } -> castling +2 "h8"

    | _ -> baseMove

  let private movePieceWithoutFinalKingCheck (pieceLocation: SquareNotation) (targetLocation: SquareNotation) (game: Game) : Result<Game, string> =
    let isOccupied square =
      game.Board |> Map.containsKey square

    let tryFindPieceAt square =
      game.Board |> Map.tryFind square

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

    let checkTarget targetSquare turn =
      let targetPiece = tryFindPieceAt targetSquare
      match targetPiece with
      | Some { Color = targetColor } when targetColor = turn
        -> Error $"move to {targetSquare.Notation} not allowed: square occupied by own piece"
      | _
        -> Ok targetPiece

    let checkPathFree insidePath targetSquare =
      let occupiedSquares =
        insidePath
        |> List.filter isOccupied
        |> List.rev
      match occupiedSquares with
      | [] -> Ok ()
      | [x] -> Error $"move to {targetSquare.Notation} not allowed: {x.Notation} occupied"
      | x::_ -> Error $"move to {targetSquare.Notation} not allowed: {x.Notation} occupied"

    let checkPieceMove { Color = turn; Piece = piece } move targetPiece targetSquare =
      match piece, move with
      | Knight, Jump
      | Queen, Forward
      | Rook, Forward
      | King, Forward
      | King, Diagonal { NumberOfSquares = 1 }
      | King, Rectilinear { NumberOfSquares = 1 }
        -> Ok ()

      | Queen, Diagonal { InsidePath = insidePath }
      | Bishop, Diagonal { InsidePath = insidePath }
      | Rook, Rectilinear { InsidePath = insidePath }
      | Queen, Rectilinear { InsidePath = insidePath }
        -> checkPathFree insidePath targetSquare

      | King, Castling x ->
        monad' {
          do! checkPathFree x.InsidePath targetSquare
            |> Result.mapError (fun err -> err.Replace("move", "castling"))
          return!
            tryFindPieceAt x.RookSquare
            |> Option.filter (fun x -> x.Piece = Rook && x.Color = turn)
            |> Option.map (fun _ -> ())
            |> toResult $"castling to {targetSquare.Notation} not allowed: no rook at {x.RookSquare.Notation}"
        }

      | Pawn, Forward ->
        // Pawn cannot capture forward -> targetSquare must be free
        match targetPiece with
        | None -> Ok ()
        | _ -> Error $"move to {targetSquare.Notation} not allowed: square occupied"

      | Pawn, Diagonal { NumberOfSquares = 1 } ->
        // Capture adversary piece?
        match targetPiece with
        | Some { Color = targetColor } when targetColor <> turn
          -> Ok ()
        | _
          -> Error $"move to {targetSquare.Notation} not allowed: no piece to capture by Pawn"

      | _ -> Error $"move to {targetSquare.Notation} not allowed for {piece}"

    monad' {
      let! (pieceSquare, targetSquare) = checkSquaresDistinct

      let! movedPiece =
        tryFindPieceAt pieceSquare |> toResult $"no piece at {pieceLocation}"
        >>= checkTurnToPlay

      let! targetPiece = checkTarget targetSquare game.Turn

      let move = computeMove pieceSquare targetSquare movedPiece.Color
      do! checkPieceMove movedPiece move targetPiece targetSquare

      let promotedPiece =
        match (movedPiece, targetSquare) with
        | { Color = White; Piece = Pawn }, { Rank = Rank._8 } -> ColoredPiece.parse '♕'
        | { Color = Black; Piece = Pawn }, { Rank = Rank._1 } -> ColoredPiece.parse '♛'
        | _ -> movedPiece

      // TODO move Rook when Castling

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