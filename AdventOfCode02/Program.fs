open System
open System.Collections.Generic
open System.Text.RegularExpressions

let matchRx = Regex(@"(\d+)-(\d+) (\w): (\w+)", RegexOptions.Compiled)

let readlines (fn) : seq<string> =
    System.IO.File.ReadLines(fn)

let buildHistogram (input:string) : Dictionary<char, int> =
    let mapping = Dictionary<char, int>()
    for ch in input do // we could go functional, but why?
        mapping.[ch] <- mapping.GetValueOrDefault(ch, 0) + 1
    mapping
        

let lineMatchPass line =
    let captures = matchRx.Match(line).Groups
    let min = captures.Item(1).Value |> Int32.Parse
    let max = captures.Item(2).Value |> Int32.Parse
    let target = captures.Item(3).Value.[0]
    let histogram = buildHistogram (captures.Item(4).Value)
    let value = histogram.GetValueOrDefault(target, 0)
    value >= min && value <= max

[<EntryPoint>]
let main argv =
    let result = readlines argv.[0]
                    |> Seq.filter lineMatchPass
                    |> Seq.length
    printfn "%d" result |> ignore
    0