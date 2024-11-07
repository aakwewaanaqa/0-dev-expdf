module expdf.Tests.EatTests

open expdf.modules.Unions
open expdf.modules.Sourcing
open expdf.modules.Parsing

let say x =
    printf $"{x}"

let ``testEatCount`` () =
    "apple"
    |> Sources.inString
    |> eat (Count 2)
    |> Results.onPassed (fun r -> r |> Remains.logValue)
    |> ignore
    ()

let ``testEatLine`` () =
    "
Ponito:\rHey!\nThis is from Taiwan.\r\nHow are you doing?\r\r
I was wondering, if we can meet up.\n
Have some fun!\r
That would be nice.
    "
    |> Sources.inString
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> eat Line |> Results.onPassed Remains.logValue
    |> ignore
    ()

let ``testEnding`` () =
    "
if [ -z $1 ]; then
    echo -e '第1個參數不能是空的'
fi
if [ -z $2 ]; then
    echo -e '第2個參數不能是空的'
fi"
    |> Sources.inString
    |> eat (Ending "fi") |> Results.onPassed Remains.logValue
    |> ignore
    ()