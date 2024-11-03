namespace expdf

open System.IO
open Microsoft.FSharp.Core
open expdf.StreamsModule
open expdf.BytesModule

module Program =
    [<EntryPoint>]
    let main args =
        use file = File.Open(args.[0], FileMode.Open)
        let std input = printfn $"{input}"
        file |> adv 15 |> Option.map toUTF8 |> std
        0