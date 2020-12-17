type Grid = char[][]

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)


let isOccupied char =
    match char with
    | '#' -> 1
    | 'L' -> 0
    | '.' -> 0
    | _ -> failwith "Unknown item"

let printMatrix (grid:char[][]) =
    for row in grid do
        for cell in row do
            printf "%c" cell
        printfn ""
    printfn ""
    printfn ""

let safeCount (grid:char[][]) (x:int) (y:int) =
        if y < 0 || x < 0 || y >= grid.Length || x >= grid.[y].Length then
            0
        else
            isOccupied grid.[y].[x]

let adjacencyCount (grid:char[][]) y x =
    let mutate = [| -1; 0; 1 |]
    seq {
        for dy in mutate do
            for dx in mutate do
                if dx + dy = 0 then // do not self count
                    yield 0
                else
                    yield safeCount grid (x+dx) (y+dy)
    } |> Seq.sum

let evalCell (grid:char[][]) x y (cell:char) =
    let count = adjacencyCount grid x y
    if cell = 'L' && count = 0 then // rule 1
        '#'
    else if cell = '#' && count >= 4 then // rule 2
        'L'
    else // no change
        cell

let evalState curState =
    printMatrix curState
    
    curState
    |> Array.mapi (Array.mapi << (evalCell curState))
    
let rec eval prevState curState =
    let nextState = evalState curState
    if nextState = prevState then
        nextState
    else
        eval curState nextState

[<EntryPoint>]
let main argv =
    let startingGrid = readlines argv.[0]
                        |> Seq.map (fun x -> x.ToCharArray())
                        |> Seq.toArray
    let initialState = evalState startingGrid
    let finalState = eval startingGrid initialState
    
    let countOccupied state =
        state
        |> Array.map (Array.sumBy isOccupied)
        |> Array.sum

    printfn "Answer: %d" (countOccupied finalState)
    0