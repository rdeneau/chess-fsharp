module Chess

type Game = Game

type Coordinate = string

let move (piece: Coordinate) (to_square: Coordinate) (game: Game) : Result<Game, string> =
  failwith "TODO"