module AdventOfCode.Program01

open System

let choose m n =
    let rec fC prefix m from = seq {
        let rec loopFor f = seq {
            match f with
            | [] -> ()
            | x::xs ->
                yield (x, fC [] (m-1) xs)
                yield! loopFor xs
        }
        if m = 0 then yield prefix
        else
            for (i, s) in loopFor from do
                for x in s do
                    yield prefix@[i]@x        
    }
    fC [] m [0..(n-1)]
let readlines (filename:string) : seq<string> =
    System.IO.File.ReadLines(filename)

let deArray (source:int[]) (indexes:seq<int>) =
    indexes |> Seq.map (fun x -> source.[x])
    
let product x =
    Seq.reduce (fun a b -> a * b) x

[<EntryPoint>]
let main argv =
    // define constants
    let TARGET_SUM = 2020
    let MATCH_COUNT = 3 //2
    
    // run
    let seq = (readlines argv.[0]) |> Seq.map Int32.Parse
    let sourceItems = Seq.toArray seq
    let matchedElement = (
        choose MATCH_COUNT sourceItems.Length // generate combinations
            |> Seq.map (deArray sourceItems) // combination -> elements
            |> Seq.filter (fun items -> (Seq.sum items) = TARGET_SUM)// filter for elements with proper sum
            |> Seq.tryHead
        )
    match matchedElement with
    | None -> 1 // error in this case
    | Some(x) -> 
        printfn "%i" (product x) |> ignore // print results, return 0
        0