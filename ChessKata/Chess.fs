module Chess

open FSharpPlus
open Helpers

type Game = Game

type Coordinate = string

module Coordinate =
  let parse (coordinate: Coordinate) =
    match coordinate with
    | Regex @"([a-h])([1-8])" [ col; row ] -> Ok [ col; row ]
    | _ -> Error "invalid coordinate"

let move (piece: Coordinate) (square: Coordinate) (game: Game) : Result<Game, string> =
  monad' {
    let! _piece  = Coordinate.parse piece  |> Result.mapError (fun x -> $"{x} ({nameof piece})")
    and! _square = Coordinate.parse square |> Result.mapError (fun x -> $"{x} ({nameof square})")
    return game
    }

let tmp = Game |> move "a1" "xx"