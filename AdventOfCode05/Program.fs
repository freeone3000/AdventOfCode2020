let readlines fn : seq<string> =
    System.IO.File.ReadLines(fn)
    

let rec binSearch pos min max lowSym highSym (def:string) =
    let mid = (min + max) / 2
    if pos+1 >= def.Length then
        if def.[pos] = lowSym then
            min
        else
            max
    else
        if def.[pos] = lowSym then
            binSearch (pos+1) min mid lowSym highSym def
        else
            binSearch (pos+1) (mid+1) max lowSym highSym def
    
let maxRow = 127
let defToRow = binSearch 0 0 maxRow 'F' 'B'
let maxCol = 7
let defToCol = binSearch 0 0 maxCol 'L' 'R'
let defToId (def:string) =   
    let row = defToRow (def.Substring(0, 7))
    let col = defToCol (def.Substring(7))
    row * 8 + col

[<EntryPoint>]
let main argv =
    printfn "Sanity: 357 = %d" (defToId "FBFBBFFRLR")
    
    let discontToId (x:int[]) =
        if x.[0] + 1 <> x.[1] then
            x.[0] + 1
        else if x.[1] + 1 <> x.[2] then
            x.[1] + 1
        else
            -1
    // alternate solution: you know, I bet we could just count to 800 and check every one.
    let result = readlines argv.[0]
                |> Seq.map defToId
                |> Seq.sort
                |> Seq.windowed 3 // big o? the giant robot?
                |> Seq.filter (fun x -> x.[0] + 1 <> x.[1] || x.[1] + 1 <> x.[2]) // find discontinuity
                |> Seq.map discontToId
                |> Seq.head
    printfn "%d" result
    0