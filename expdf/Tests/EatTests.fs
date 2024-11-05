module expdf.Tests.EatTests

open expdf.modules.Unions
open expdf.modules.Sourcing
open expdf.modules.Parsing

let say x =
    printf $"{x}"

let ``testEatCount`` () =
    "apple"
    |> inString
    |> eat (Count 2)
    |> onPassed (fun r -> r |> report)
    |> ignore

    ()

let ``testEatLine`` () =
    "From teacher:\r\n Hello this is your teacher.\r\n How are you doing?"
    |> inString
    |> eat Line |> goOn
    |> eat Line |> goOn
    |> eat Line
    |> onPassed (fun r -> r |> report)
    |> ignore

    ()

