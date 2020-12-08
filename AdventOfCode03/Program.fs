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
    let mutable treeCount = 0
    let mutable xpos = 0
    let mutable ypos = 0
    while ypos < map.Length do
        if map.Get xpos ypos |> isTree then
            treeCount <- treeCount + 1
        xpos <- xpos + right
        ypos <- ypos + down
    treeCount

[<EntryPoint>]
let main argv =
    let right = argv.[0] |> Int32.Parse
    let down = argv.[1] |> Int32.Parse
    
    let map = TobogganMap(readlines "map.txt")
    let f = treeCount map
    
    let treeCount = f right down
    printfn "%d" treeCount |> ignore
    0