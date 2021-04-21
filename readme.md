# Chess Code Kata

Implement a [Chess](https://en.wikipedia.org/wiki/Chess) game with the features beneath.

## 1. Add

Add a *piece* at a given *square* in the classical 8x8 *board*

- **Piece** is specified by its *Symbol* that also indicates its *Color*:

| Color   | King | Queen | Rook | Bishop | Knight | Pawn |
|---------|------|-------|------|--------|--------|------|
| *White* | ♔    | ♕     | ♖    | ♗      | ♘      | ♙    |
| *Black* | ♚    | ♛     | ♜    | ♝      | ♞      | ♟    |

- **Board** is a 8×8 squares, each defined by its:
  - row called [*rank*](https://en.wikipedia.org/wiki/Glossary_of_chess#ranks) and denoted *1* to *8*
  - column called [*file*](https://en.wikipedia.org/wiki/Glossary_of_chess#files) and denoted *a* to *h*

|       | ａ | ｂ | ｃ | ｄ | ｅ | ｆ | ｇ | ｈ |
|-------|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| **8** | ♜ | ♞ | ♝ | ♛ | ♚ | ♝ | ♞ | ♜ |
| **7** | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ |
| **6** |   |   |   |   |   |   |   |   |
| **5** |   |   |   |   |   |   |   |   |
| **4** |   |   |   |   |   |   |   |   |
| **3** |   |   |   |   |   |   |   |   |
| **2** | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ |
| **1** | ♖ | ♘ | ♗ | ♕ | ♔ | ♗ | ♘ | ♖ |

- **Square** is specified by a `string` of 2 characters `{file}{rank}`, e.g. "a5"<br>
  → see [algebraic notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess))

## 2. Move

Move a piece specified by its square to a destination square, verifying the rules of the game:

- [x] Verify that there is a piece at the given square and of the **Turn** color, *White* or *Black*
- [x] Respect the move allowed for the given piece
- [x] Handle *Pawn* promoted to *Queen*
- [x] Handle blockage on the way by another piece
- [x] Handle capture of an adversary piece, including *Pawn* capturing in diagonal

## 3. Check

- [x] Detect [*Check*](https://en.wikipedia.org/wiki/Check_(chess))
- [x] Reject board missing either *King*
  - ⚠️ Might imply adapting previous tests !
- [x] Reject any move ending up in own *Check*

## 4. Extra moves *(check or past moves needed)*

- [x] Handle [castling](https://en.wikipedia.org/wiki/Castling)
- [x] Handle pawn capture "[en passant](https://en.wikipedia.org/wiki/En_passant)"

## 5. Mate

- [x] Detect [Mate](https://en.wikipedia.org/wiki/Checkmate)
- [x] Detect [Stalemate](https://en.wikipedia.org/wiki/Stalemate)
