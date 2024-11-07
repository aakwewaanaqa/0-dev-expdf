module expdf.modules.Parsing

open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open Newtonsoft.Json
open Unions
open Sourcing


[<Struct>]
type Remain(_left: source, _value: source) =
    member this.left = _left
    member this.value = _value

module Remains =
    let logValue (r: Remain) =
        printf ""
        printf $"v <-'{r.value |> Sources.toString}'\n"
        printf $"l <-'{r.left |> Sources.toString}'\n"

type result =
     | Passed of Remain
     | Fail of source

module Results =
    let ifPassed (r: result) =
        match r with
        | Passed r -> r.left
        | Fail _ -> failwith "failed..."

    let onPassed (fn: Remain -> unit) (r: result) : source =
        match r with
        | Passed r ->
            fn r
            r.left
        | Fail _ -> failwith "failed..."


let eat (m: mode) (s: source) : result =
    match m with

    | Count c ->
        if s.Length < c then
            Fail s
        else
            Passed (Remain(s[c..], s[..c - 1]))

    | Ending ending ->
        let str = s |> Sources.toString
        let i = str.IndexOf(ending)
        let offset = ending.Length
        let value = str.Substring(0, i + offset) |> Sources.inString;
        let remain = str.Substring(i + offset, str.Length - i - offset) |> Sources.inString;
        if i > -1 then
            Passed (Remain(remain, value))
        else
            Fail s


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
        Passed (Remain(s[v.Length ..], v))

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
        Passed (Remain(s[v.Length ..], v))

    | Pattern regex ->
        let str = s |> Sources.toString
        let m = regex.Match str
        let v = m.Value |> Sources.inString
        match m.Success, m.Index with
        | true, 0 -> Passed (Remain(s[v.Length ..], v))
        | _ -> Fail s