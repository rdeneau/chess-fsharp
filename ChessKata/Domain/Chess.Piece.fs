namespace ChessKata.Domain

type PieceSymbol = char

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type Color = Black | White

module Color =
  let toggle color : Color =
    match color with
    | White -> Black
    | Black -> White

type ColoredPiece = { Color: Color; Piece: Piece; Symbol: PieceSymbol }

module ColoredPiece =
  let tryParse (symbol: PieceSymbol) =
    match symbol with
    | '♔' -> Some { Symbol = symbol; Color = White; Piece = King }
    | '♕' -> Some { Symbol = symbol; Color = White; Piece = Queen }
    | '♖' -> Some { Symbol = symbol; Color = White; Piece = Rook }
    | '♗' -> Some { Symbol = symbol; Color = White; Piece = Bishop }
    | '♘' -> Some { Symbol = symbol; Color = White; Piece = Knight }
    | '♙' -> Some { Symbol = symbol; Color = White; Piece = Pawn }
    | '♚' -> Some { Symbol = symbol; Color = Black; Piece = King }
    | '♛' -> Some { Symbol = symbol; Color = Black; Piece = Queen }
    | '♜' -> Some { Symbol = symbol; Color = Black; Piece = Rook }
    | '♝' -> Some { Symbol = symbol; Color = Black; Piece = Bishop }
    | '♞' -> Some { Symbol = symbol; Color = Black; Piece = Knight }
    | '♟' -> Some { Symbol = symbol; Color = Black; Piece = Pawn }
    | _ -> None

  let parse (symbol: PieceSymbol) =
    match tryParse symbol with
    | Some square -> square
    | None -> failwith "invalid coordinate"
