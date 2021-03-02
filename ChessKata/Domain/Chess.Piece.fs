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
    let coloredPiece color piece =
      { Symbol = symbol; Color = color; Piece = piece }
    match symbol with
    | '♔' -> Some <| coloredPiece White King
    | '♕' -> Some <| coloredPiece White Queen
    | '♖' -> Some <| coloredPiece White Rook
    | '♗' -> Some <| coloredPiece White Bishop
    | '♘' -> Some <| coloredPiece White Knight
    | '♙' -> Some <| coloredPiece White Pawn
    | '♚' -> Some <| coloredPiece Black King
    | '♛' -> Some <| coloredPiece Black Queen
    | '♜' -> Some <| coloredPiece Black Rook
    | '♝' -> Some <| coloredPiece Black Bishop
    | '♞' -> Some <| coloredPiece Black Knight
    | '♟' -> Some <| coloredPiece Black Pawn
    | _    -> None

  let parse (symbol: PieceSymbol) =
    match tryParse symbol with
    | Some square -> square
    | None -> failwith "invalid coordinate"
