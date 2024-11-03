module expdf.Tests.EatTests

open expdf.Parsing

let say x =
    printf $"{x}"

let ``testEatCount`` () =
    ['a'; 'p'; 'p'; 'l'; 'e']
    |> asSource
    |> eat (Count 2)
    |> say