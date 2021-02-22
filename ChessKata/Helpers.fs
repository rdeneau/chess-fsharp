module Helpers

open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let toResult errorIfNone = function
  | Some x -> Ok x
  | None -> Error errorIfNone

let enumValues<'a when 'a :> Enum> = (Enum.GetValues(typeof<'a>) :?> ('a [])) |> Array.toList

let sign x =
    match x with
    | x when x < 0 -> -1
    | 0 -> 0
    | _ -> 1

/// Remove first and last elements in the given list
let trimList =
  function
  | [] | [_] | [_;_] -> []
  | ls -> ls |> List.tail |> List.rev |> List.tail |> List.rev