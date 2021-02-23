# Chess Code Kata

Implement a [Chess](https://en.wikipedia.org/wiki/Chess) game with the features beneath.

## 1. Add

Add a piece at a given square in the classical 8x8 board

- **Piece** is specified by its *Symbol* that also indicates its *Color*:

| Color   | King | Queen | Rook | Bishop | Knight | Pawn |
|---------|------|-------|------|--------|--------|------|
| *White* | ♔    | ♕     | ♖    | ♗      | ♘      | ♙    |
| *Black* | ♚    | ♛     | ♜    | ♝      | ♞      | ♟    |

- **Square** is specified by a `string` of 2 characters `{file}{rank}`, e.g. "a5"<br>
  → see [algebraic notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess))

## 2. Move

Move a piece specified by its square to a destination square, verifying the rules of the game:

- [x] Verify that there is a piece at the given square and of a the **Turn** color, *White* or *Black*
- [x] Respect the move allowed for the given piece
- [x] Handle *Pawn* promoted to *Queen*
- [x] Handle blockage on the way by another piece
- [x] Handle capture of an adversary piece, including *Pawn* capturing in diagonal

## 3. Check

- [ ] Detect [*Check*](https://en.wikipedia.org/wiki/Check_(chess))
- [ ] Reject *King* move ending up in *Check*

## 4. Move extra rules

- [ ] Handle [castling](https://en.wikipedia.org/wiki/Castling) move
- [ ] Handle pawn capture [en passant](https://en.wikipedia.org/wiki/En_passant)

## 5. End game

- [ ] Detect **checkmate**
