open System

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)

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

let sumTo arr =
    choose 2 (Array.length arr)
    |> Seq.map (fun x -> arr.[x.[0]] + arr.[x.[1]])
    |> Set.ofSeq

[<EntryPoint>]
let main argv =
    let window = match Int32.TryParse(argv.[0]) with
                 | (true, x) -> x
                 | (false, _) -> failwith "First argument must be an integer"
    let lines = readlines argv.[1]
                |> Seq.map Int64.Parse
                |> Seq.toArray
    
    let calc idx = // sum last 'window' elements as a set.
        if idx < window then
            Set.empty
        else
            sumTo lines.[idx-window..idx-1]
            
    
    let x = 35
    Array.iter (printf "%d,") lines.[x-window-1..x-1]
    printfn "\n--"
    Seq.iter (printf "%d,") (calc x)
    printfn "\n--"
        
    lines
    |> Array.mapi (fun idx elem -> elem, Set.contains elem (calc idx))  // determine if item exists in the set so far
    |> Array.skip window // skip preamble, which is conveniently also window-sized
    |> Array.filter (fun (_, x) -> not x)
    |> Array.iter ((printfn "%d") << fst)
    0