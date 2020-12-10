open FSharp.Collections

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)
    
let parseGroups (lines:seq<string>) =
    let mutable groups = []
    let mutable curGroup = None
    
    for line in lines do
        if line.Length = 0 then
            groups <- curGroup :: groups
            curGroup <- None
        else
            let person = Set<char>(seq { for ch in line -> ch})
//            curGroup <- Set.union curGroup person
            curGroup <- match curGroup with
                        | None -> Some person
                        | Some group -> Some (Set.intersect group person)
    (curGroup :: groups)
    
[<EntryPoint>]
let main argv =
    let counts = parseGroups (readlines argv.[0])
                |> List.map (fun group -> match group with
                                           | None -> 0
                                           | Some s -> s.Count)
    printfn "Count:\n%d" (List.sum counts)
    0