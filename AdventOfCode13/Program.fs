

open System.Text.RegularExpressions
let readlines (fn:string):seq<string> =
    System.IO.File.ReadLines(fn)

let parseExpression (line:string) =
    Regex.Split(line, ",")
    |> Array.filter (fun x -> x <> "x")
    |> Array.map int

[<EntryPoint>]
let main argv =
    let determineDifference (target:int) (bus:int) =
        bus - (target % bus)
    
    let lines = readlines argv.[0]
                |> Seq.toArray
    let timestamp = int lines.[0]
    let busses = parseExpression lines.[1]
    
    let result = busses
                 |> Array.map (fun bus -> bus, (determineDifference timestamp bus))
                 |> Array.minBy snd
                 
    printfn ("Result: %d") ((fst result) * (snd result)) // bus id * number of minutes to wait, as requested
    
    0