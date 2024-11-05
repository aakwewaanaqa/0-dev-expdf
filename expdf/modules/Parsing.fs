module expdf.modules.Parsing

open System.Text.Json
open System.Text.Json.Nodes
open Microsoft.FSharp.Core
open Newtonsoft.Json
open Unions
open Sourcing

[<Struct>]
type Result(_passed: bool, _left: source, _value: source) =
    member this.passed = _passed
    member this.left = _left
    member this.value = _value
    new(left: source) = Result(false, left, [])

let goOn (r: Result) =
    if not r.passed then
        failwith "previous result doesn't passed"
    else
        r.left

let onPassed (fn: Result -> unit) (r: Result) : source =
    if not r.passed then
        failwith "previous result doesn't passed"
    else
        fn r
        r.left

let report (r: Result) =
    printf "("
    printf $"{r.passed}, "
    printf $"'{r.left |> toString}', "
    printf $"->'{r.value |> toString}'"
    printf ")"

let eat (m: mode) (s: source) : Result =
    match m with

    | Count c ->
        if s.Length < c then
            Result(s)
        else
            Result(true, s[c..], s[..c - 1])

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
                let c =
                    s[i]
                    |> (function
                    | Char c -> c
                    | Byte b -> b |> char)

                let adv = i + 1
                let append = acc @ [ s[i] ]
                let isCR = c = '\r'
                let isLF = c = '\n'

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
