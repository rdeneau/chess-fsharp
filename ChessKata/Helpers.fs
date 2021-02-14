module Helpers

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let toResult errorIfNone = function
  | Some x -> Ok x
  | None -> Error errorIfNone
