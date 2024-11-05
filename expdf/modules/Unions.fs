module expdf.modules.Unions

open System.Text.RegularExpressions

type a =
    | Byte of byte
    | Char of char

type mode =
    | Count of int
    | Ending of a
    | Endings of list<a>
    | Line
    | Pattern of Regex