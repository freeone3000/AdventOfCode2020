open System
open System.Collections
open System.Collections.Generic

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)
    
let printDict (x:Dictionary<uint64, uint64>): unit =
    for value in x |> Seq.cast<KeyValuePair<uint64, uint64>> do
        printfn "%s" (value.ToString())

[<EntryPoint>]
let main argv =
    let mutable oneDiff = 0
    let mutable twoDiff = 0
    let mutable threeDiff = 0
    
    let sortedInput = readlines argv.[0]
                      |> Seq.map UInt64.Parse
                      |> Seq.sort
                      |> Seq.toList
    List.fold (fun last cur ->
        let diff = cur - last
        match diff with
        | 1uL -> oneDiff <- oneDiff + 1
        | 2uL -> twoDiff <- twoDiff + 1
        | 3uL -> threeDiff <- threeDiff + 1
        | _ -> failwith "invalid combination"
        cur
        )
        0uL
        sortedInput
    |> ignore
    
    threeDiff <- threeDiff + 1 // three bigger than largest adapter
    printfn "Ones:\t%d\nThrees:\t%d" oneDiff threeDiff
    printfn "Result:\t%d\n" (oneDiff * threeDiff)
    
    let mutable num_paths = Dictionary<uint64, uint64>()
    let targetItem = (List.max sortedInput) + 3uL
    num_paths.[targetItem] <- 1uL
    let augInput = [0uL]@sortedInput@[targetItem] |> List.toArray // reverse sorted list, with our target, to an array
    let maxIdx = Array.length augInput - 1
    
    let memoizedNeighbours i_val j =
        let j_val = augInput.[j]
//        printfn "Comparing %d to %d" i_val j_val // DEBUG
        let neighbours = match j_val - i_val with
                         | 1uL | 2uL | 3uL -> num_paths.[j_val] 
                         | _ -> 0uL
//        printfn "Found neighbors: %d" neighbours // DEBUG
        neighbours
    
    for i = maxIdx - 1 downto 0 do
        let i_val = augInput.[i]
        let sum = seq { (i+1) .. (min (i+3) maxIdx) }
                  |> Seq.map (memoizedNeighbours i_val) 
                  |> Seq.sum
        num_paths.[i_val] <- sum
        
//    printDict num_paths
    printfn "Number of paths: %d" num_paths.[augInput.[0]]
    0