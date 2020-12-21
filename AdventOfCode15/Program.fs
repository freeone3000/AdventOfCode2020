open System

let rec calculate idx last goal entries =
//    Map.iter (printf "%d: %d; ") entries
//    printfn ""
    if idx % 100000 = 0 then
        printfn "Iter: %d" idx
    
    if idx >= goal then
        last
    else
        let next = match Map.tryFind last entries with
                   | None -> 0
                   | Some a -> idx - a - 1
        calculate (idx+1) next goal (Map.add last (idx-1) entries)


[<EntryPoint>]
let main argv =
    printf "Enter the known numbers: "
    let input = Console.ReadLine().Split(",")
                |> Array.map Int32.Parse
    if Array.length input < 3 then
        failwith "Syntax for input is 1,3,5"
        
    let entries = input
                  |> Array.mapi (fun x y -> (y, x))
                  |> Map.ofArray
    
//    let targetLength = 2020
    let targetLength = 30000000
    let answer = calculate (Map.count entries) (Array.last input) targetLength entries 
    printfn "Answer: %d" answer
    0