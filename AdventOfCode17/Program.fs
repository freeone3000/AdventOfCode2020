// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FSharp.Collections

type Coordinates = int * int * int

let CYCLES = 6

// The instructions specify an infinite grid. However, since we are executing a fixed number of cycles,
// it will be impossible for any "on" value to be more than a certain distance away from the grid "center" (our initial state).
// First attempt will assume that this distance is equal to the cycle count.
let HORIZON = CYCLES

let ON = 1 // '#'
let OFF = 0 // '.'

type SmartGrid(initial: int[,]) =
    let mutable stepsGenerated = 0 // For printing of bounds
    let mutable backing = Array3D.zeroCreate (HORIZON + HORIZON + Array2D.length1 initial) (HORIZON + HORIZON + Array2D.length2 initial) (HORIZON + HORIZON + 1)
        
    let bget x y z =
        backing.[x + CYCLES, y + CYCLES, z + CYCLES]
     
    let bset x y z value =
        backing.[x + HORIZON, y + HORIZON, z + HORIZON] <- value
        
    do
        // initialize and copy
        for x = 0 to Array2D.length1 initial - 1 do
            for y = 0 to Array2D.length2 initial - 1 do
                    bset x y 0 initial.[x,y]
    
    member this.generateInPlace () =
//        stepsGenerated <- stepsGenerated + 1
        ()

    member this.print () =
        for z = 0 to stepsGenerated do
            printfn "z = %d" z
            for x = 0 to Array2D.length1 initial - 1 + stepsGenerated do
                for y = 0 to Array2D.length2 initial - 1 + stepsGenerated do
                    printf (if bget x y z = 1 then "#" else ".")
                printfn ""
        
    member this.Count () =
        let mutable sum = 0
        for x = 0 to Array3D.length1 backing - 1 do
            for y = 0 to Array3D.length2 backing - 1 do
                for z = 0 to Array3D.length3 backing - 1 do
                    if backing.[x, y, z] = 1 then sum <- sum + 1
        sum

let readFileToGrid (fn: string) : int[,] =
    let lines = Array.map (fun (x:String) -> x.Trim()) (System.IO.File.ReadAllLines(fn))
    Array2D.init (Array.length lines) (String.length lines.[0]) (fun x y -> if lines.[x].[y] = '#' then 1 else 0)

[<EntryPoint>]
let main argv =
    let initialState = readFileToGrid argv.[0]
    let grid = SmartGrid initialState
    grid.generateInPlace ()
    grid.print ()
//    printfn "Count: %d" (grid.Count ())
    0