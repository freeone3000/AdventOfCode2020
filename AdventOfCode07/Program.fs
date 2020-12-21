open System
open System.Text.RegularExpressions

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)


type Contents =
    {
        number : int
        color : string
    }
type Bag =
    {
        color : string
        contents : Contents[]
    }

let bagToString bag =
    let contentsToString contents =
        let mapped = Array.map (fun x -> sprintf "Contains: %d %s" x.number x.color) contents
        String.Join("\n", mapped)
        
    sprintf "Color: %s\n%s\n----" bag.color (contentsToString bag.contents)
    

let parse (line:string) =
    let parseContent parsed color =
        match parsed with
        | (true, num) -> { number = num; color = color }
        | (false, _) -> failwith "Could not parse string as int"

    let bagsRegex = Regex(@"(\d+)\s+(.*?)\s+bags?")
    
    let parts = line.Split(" bags contain ")
    let color = parts.[0]
    let contents =
        seq {
            for m in bagsRegex.Matches(parts.[1]) do
                yield Int32.TryParse(m.Groups.Item(1).Value),
                        m.Groups.Item(2).Value.TrimEnd('.')
            } 
            |> Seq.map (fun (x, y) -> parseContent x y)
            |> Seq.toArray
    { color = color ; contents = contents }
    

let bagSearch needle bags =
    let rec search seen needle bag =
        if Set.contains bag.color seen then
            0
        else if bag.color = needle then
            1
        else
            bag.contents
            |> Array.map (fun contents -> Map.find contents.color bags)
            |> Array.sumBy (search (Set.add bag.color seen) needle)
    
    bags
    |> Map.filter (fun _ v -> v.color <> needle) // trim top-level self
    |> Map.map (fun _ -> search Set.empty needle) // bag -> number of paths

let rec bagFill needle bags =
    let bag = Map.find needle bags
    let children = bag.contents
                   |> Array.map (fun contents -> contents.number * (bagFill contents.color bags))
                   |> Array.sum
    1 + children // include self

[<EntryPoint>]
let main argv =
    let bags = readlines argv.[0]
               |> Seq.map parse
               |> Seq.map (fun bag -> bag.color, bag)
               |> Map.ofSeq
    
    // part 01
    bagSearch "shiny gold" bags
    |> Map.filter (fun _ count -> count > 0)
    |> Map.count
    |> printfn "Contents for a shiny gold bag: %d"
    
    // part 02 - why are these *min* instead of *max*? this stretches credulity
    printfn "Total number of bags in a shiny gold bag: %d" ((bagFill "shiny gold" bags) - 1) // subtract top level
    
    0