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
    

[<EntryPoint>]
let main argv =
    let bags = readlines argv.[0]
               |> Seq.map parse
               |> Seq.map (fun bag -> bag.color, bag)
               |> Map.ofSeq
    bagSearch "shiny gold" bags
    |> Map.toSeq
    |> Seq.map (fun (_, count) -> if count > 0 then 1 else 0)
    |> Seq.sum
    |> printfn "Bags: %d"
    
    
    0 // return an integer exit code