module expdf.BytesModule

open System.Text

let toUTF8 (bs: byte[]) : string =
    if bs = null || bs.Length = 0 then ""
    else Encoding.UTF8.GetString(bs)