open System

let rec calculate goal entries =
    let length = List.length entries
    let target = List.last entries
    
    if length >= goal then
        target
    else
        match List.tryFindIndexBack (fun x -> x = target) (List.truncate (length-1) entries) with
        | None -> calculate goal (entries@[0])
        | Some a -> calculate goal (entries@[length - a - 1])

[<EntryPoint>]
let main argv =
    printf "Enter the known numbers: "
    let numbers = Console.ReadLine().Split(",")
                  |> List.ofArray
                  |> List.map Int32.Parse
    if List.length numbers < 3 then
        failwith "Syntax for input is 1,3,5"
    printfn "Answer: %d" (calculate 2020 numbers)
    0