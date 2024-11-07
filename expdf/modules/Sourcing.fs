module expdf.modules.Sourcing

open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core
open Unions

type source = a list

module Sources =
    let inChars (l: char list) : source =
        [ for c in l do
              yield Char c ]

    let inBytes (l: byte list) : source =
        [ for c in l do
              yield Byte c ]

    let inString (l: string) : source =
        [ for c in l do
              yield Char c ]

    let toString (s: source) : string =
        let builder = new StringBuilder()

        do
            for a in s do
                match a with
                | Byte b -> builder.Append(b |> char) |> ignore
                | Char c ->
                    match c with
                    | '\n' -> builder.Append("\\n") |> ignore
                    | '\r' -> builder.Append("\\r") |> ignore
                    | c -> builder.Append(c) |> ignore

        builder.ToString()