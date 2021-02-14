# Chess Code Kata

Implement a [Chess](https://en.wikipedia.org/wiki/Chess) game with the features beneath.

## 1. Add

Add a piece at a given square in the classical 8x8 board

- **Piece** is specified by its **symbol** that also indicates its color:

| Color | King | Queen | Rook | Bishop | Knight | Pawn |
|-------|------|-------|------|--------|--------|------|
| White | ♔    | ♕     | ♖    | ♗      | ♘      | ♙    |
| Black | ♚    | ♛     | ♜    | ♝      | ♞      | ♟    |

- **Square** is specified by a `string` of 2 characters `{file}{rank}`, e.g. "a5"<br>
  → see [algebraic notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess))

## 2. Move

Move a piece specified by its square to a destination square, verifying the rules of the game:

- [x] Verify that there is a piece at the given square and of a the **turn** color
- [x] Respect the move allowed for the given piece
- [ ] Handle blockage by another piece
- [ ] Handle capture of an adversary piece

### 🎁 Bonus rules

- [ ] Handle pawn capture [en passant](https://en.wikipedia.org/wiki/En_passant)
- [ ] Handle [castling](https://en.wikipedia.org/wiki/Castling) move

## 3. End game

- [ ] Detect **check**
- [ ] Detect **checkmate**
