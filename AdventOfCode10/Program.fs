open System

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)

[<EntryPoint>]
let main argv =
    let mutable oneDiff = 0
    let mutable twoDiff = 0
    let mutable threeDiff = 0
    
    readlines argv.[0]
    |> Seq.map Int32.Parse
    |> Seq.sort
    |> Seq.fold (fun last cur ->
        let diff = cur - last
        match diff with
        | 1 -> oneDiff <- oneDiff + 1
        | 2 -> twoDiff <- twoDiff + 1
        | 3 -> threeDiff <- threeDiff + 1
        | _ -> failwith "invalid combination"
        cur
        )
        0
    |> ignore
    
    threeDiff <- threeDiff + 1 // three bigger than largest adapter
    printfn "Ones:\t%d\nThrees:\t%d\n" oneDiff threeDiff
    printfn "Result:\t%d" (oneDiff * threeDiff)
    0
                