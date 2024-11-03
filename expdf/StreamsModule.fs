module expdf.StreamsModule

open System.Collections.Generic
open System.IO
open System.Text

let isEnd (s: Stream) : bool =
    match s with
    | null -> true
    | s when s.Position >= s.Position -> true
    | _ -> false

/// <summary>
///     往前讀 count 個 byte
/// </summary>
let adv (count : int) (s: Stream) : byte[] option =
    if isNull s then None
    elif s |> isEnd then None
    else
        let bf = Array.zeroCreate count
        let length = s.Read(bf, 0, count)
        Some bf[.. length - 1]

let advLine (s: Stream) : byte[] option =
    if isNull s then None
    elif s |> isEnd then None
    else
        let list = new List<byte>()
        let mutable at = s.ReadByte()
        let mutable prev = -1
        while at <> -1 && not (at = 10 || (at = 13 && prev <> 10)) do
            list.Add(byte at)
            prev <- at
            at <- s.ReadByte()
        if at <> -1 then list.Add(byte at)
        Some (list.ToArray())
