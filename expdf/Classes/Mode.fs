module expdf.Parsing

open System.Text.RegularExpressions
open Microsoft.FSharp.Core

type a =
    | Byte of byte
    | Char of char

type source = list<a>

type mode =
    | Count of int
    | Ending of a
    | Endings of list<a>
    | Line
    | Pattern of Regex

[<Struct>]
type Result(passed: bool, left: source, value: source) =
    new(left: source) = Result(false, left, [])

let asSource (l: char list) : source =
    [for c in l do yield Char c]

let eat (m: mode) (s: source) : Result =
    match m with

    | Count c ->
        if s.Length < c then
            Result(s)
        else
            Result(true, s[c..], s[..c])

    | Ending ending ->
        let rec collect (i: int) acc =
            if i >= s.Length then
                acc
            else
                let adv = i + 1
                let append = (acc @ [ s[i] ])
                let isEnding = s[i] = ending

                match isEnding with
                | true -> append
                | false -> append |> collect adv

        let v = collect 0 []
        Result(true, s[v.Length ..], v)

    | Endings endings ->
        let rec collect (i: int) (flag: bool) (acc: source) =
            if i >= s.Length then
                acc
            else
                let adv = i + 1
                let append = (acc @ [ s[i] ])
                let contains = endings |> List.contains s[i]

                match contains, flag with
                | false, true -> acc
                | true, _ -> append |> collect adv true
                | _ -> append |> collect adv flag

        let v = collect 0 false []
        Result(true, s[v.Length ..], v)

    | Line ->
        let rec collect (i: int) (isCRmet: bool) (acc: source) =
            if i >= s.Length then
                acc
            else
                let adv = i + 1
                let append = acc @ [ s[i] ]

                let c =
                    s[i]
                    |> (function
                    | Char c -> c
                    | Byte b -> b |> char)

                let isLF = c = '\n'
                let isCR = c = '\r'

                match isCRmet, isLF, isCR with
                | false, true, false -> append
                | false, false, true -> collect adv true append
                | false, false, false -> collect adv false append
                | true, true, false -> append
                | true, false, true -> acc
                | true, false, false -> acc
                | _ -> failwith "eat Line falls to here..."

        let v = collect 0 false []
        Result(true, s[v.Length ..], v)

    | Pattern regex ->
        let rec collect (i: int) (acc: source) =
            let adv = i + 1
            let append = acc @ [ s[i] ]
            let cantAppend = i >= s.Length

            let isMatch =
                if cantAppend then
                    false
                else
                    append
                    |> List.map (function
                        | Char c -> c
                        | Byte b -> b |> char)
                    |> List.toArray
                    |> string
                    |> regex.IsMatch

            if i >= s.Length then
                acc
            else
                match isMatch with
                | true -> append |> collect adv
                | false -> acc

        let v = collect 0 []

        if v.Length = 0 then
            Result(s)
        else
            Result(true, s[v.Length ..], v)
