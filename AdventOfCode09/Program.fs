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
    let lineSeq = readlines argv.[1]
                  |> Seq.map Int64.Parse
    let lines = Seq.toArray lineSeq
    
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
    
    // part 1    
    let target = lineSeq
                 |> Seq.mapi (fun idx elem -> elem, Set.contains elem (calc idx))  // determine if item exists in the set so far
                 |> Seq.skip window // skip preamble, which is conveniently also window-sized
                 |> Seq.filter (fun (_, x) -> not x)
                 |> Seq.map fst
                 |> Seq.head
    printfn "No match: %d" target
    
    // part 2
    let sumFrom idx =
        seq {
            for i = idx to Array.length lines do
                let range = lines.[idx..i]
                yield (Array.min range + Array.max range), Array.sum range 
        }
        |> Seq.takeWhile ((fun x -> x <= target) << snd)
        |> Seq.tryLast
   
    let answer = Seq.unfold (fun idx -> if idx >= Array.length lines then None else Some (sumFrom idx, idx+1)) 0
                 |> Seq.choose id
                 |> Seq.find (fun (_, sum) -> sum = target)
    printfn "Answer: %d" (fst answer)
    0