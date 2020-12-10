open FSharp.Collections

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)
    
let parseGroups (lines:seq<string>) =
    let mutable groups = []
    let mutable curGroup = Set<char>(Seq.empty)
    
    for line in lines do
        if line.Length = 0 then
            groups <- curGroup :: groups
            curGroup <- Set<char>(Seq.empty)
        else
            for ch in line do
                curGroup <- curGroup.Add(ch)
    (curGroup :: groups)
    
[<EntryPoint>]
let main argv =
    let count = parseGroups (readlines argv.[0])
                |> List.map (fun group -> group.Count)
                |> List.sum
    printfn "Count:\n%d" count
    0