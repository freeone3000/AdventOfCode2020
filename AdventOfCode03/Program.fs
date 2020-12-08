open System

let readlines (fn) : string[] =
    System.IO.File.ReadLines(fn) |> Seq.toArray

type TobogganMap(source: string[]) =
    member self.Length = source.Length
    
    member self.Get (x:int) (y:int) : char =
        if y >= source.Length then
            (char)0
        else
            let str = source.[y]
            str.[x % str.Length]

let isTree ch =
    ch = '#'

let treeCount (map:TobogganMap) right down =
    let mutable treeCount:int64 = 0L
    let mutable xpos = 0
    let mutable ypos = 0
    while ypos < map.Length do
        if map.Get xpos ypos |> isTree then
            treeCount <- treeCount + 1L
        xpos <- xpos + right
        ypos <- ypos + down
    treeCount

[<EntryPoint>]
let main argv =
    let directions = [|
        (1, 1)
        (3, 1)
        (5, 1)
        (7, 1)
        (1, 2)
    |]
    let product = Array.reduce (fun a b -> a * b)
    let map = TobogganMap(readlines argv.[0])
    let f = treeCount map
    
    directions
        |> Array.map (fun (right, down) -> f right down)
        |> product
        |> (printfn "%d")
    
    0