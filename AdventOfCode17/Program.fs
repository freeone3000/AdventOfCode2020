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
    let mutable printMinimum = (HORIZON, HORIZON, HORIZON, HORIZON)
    let mutable printMaximum = (Array2D.length1 initial + HORIZON - 1, Array2D.length2 initial + HORIZON - 1, HORIZON, HORIZON)
    let mutable cycleCount = 0
    let mutable backing = Array4D.zeroCreate (Array2D.length1 initial + HORIZON + HORIZON) (Array2D.length2 initial + HORIZON + HORIZON) (1 + HORIZON + HORIZON) (1 + HORIZON + HORIZON)
    
    do
        let bset (x, y) value =
            backing.[x + HORIZON, y + HORIZON, HORIZON, HORIZON] <- value
        // initialize and copy
        for x = 0 to Array2D.length1 initial - 1 do
            for y = 0 to Array2D.length2 initial - 1 do
                    bset (x, y) initial.[x, y]
    
    let bget (mx, my, mz, mw) =
        if (mx >= Array4D.length1 backing || my >= Array4D.length2 backing || mz >= Array4D.length3 backing || mw >= Array4D.length4 backing) || (mx < 0 || my < 0 || mz < 0 || mw < 0) then 0
        else backing.[mx, my, mz, mw]
        

          
    member this.generateInPlace () =
        let mutable minCoords = (Array4D.length1 backing, Array4D.length2 backing, Array4D.length3 backing, Array4D.length4 backing)
        let mutable maxCoords = (-1, -1, -1, -1)
        
        cycleCount <- cycleCount + 1 // update here for easier debugging
        
        // utility function
        let piecewise f (a1, b1, c1, d1) (a2, b2, c2, d2) = (f a1 a2, f b1 b2, f c1 c2, f d1 d2)
        
        let successorFunction x y z w =
            // sum neighbors
            let neighbors x y z w =
                let mutable sum = 0
                for xoff = -1 to 1 do
                    for yoff = -1 to 1 do
                        for zoff = -1 to 1 do
                            for woff = -1 to 1 do
                                if xoff <> 0 || yoff <> 0 || zoff <> 0 || woff <> 0 then
                                    sum <- sum + bget (x+xoff, y+yoff, z+zoff, w+woff)
                sum
            // calculate new status
            let ncount = neighbors x y z w
            let prev = bget (x, y, z, w)
            let newStatus = 
                if prev = ON then
                    if (ncount = 2 || ncount = 3) then ON else OFF
                else
                    if ncount = 3 then ON else OFF
            // update the min/max for print later
            if newStatus = ON then
                minCoords <- piecewise min minCoords (x, y, z, w)
                maxCoords <- piecewise max maxCoords (x, y, z, w)
            // return
            newStatus
        
        let newItem = Array4D.init (Array4D.length1 backing) (Array4D.length2 backing) (Array4D.length3 backing) (Array4D.length4 backing) successorFunction
        printMinimum <- minCoords
        printMaximum <- maxCoords
        backing <- newItem

    member this.print () =
        let (minX, minY, minZ, minW) = printMinimum
        let (maxX, maxY, maxZ, maxW) = printMaximum
        
        printfn "Starting at (%d, %d)" minX minY
        
        printfn "After %d cycles:" cycleCount
        for w = minW to maxW do
            for z = minZ to maxZ do
                printfn "\nz=%d, w=%d" (z - HORIZON) (w - HORIZON) // by convention
                for x = minX to maxX do
                    for y = minY to maxY do
                        printf (if backing.[x, y, z, w] = 1 then "#" else ".")
                    printfn ""
        printfn "\n"
        
    member this.Count () =
        let mutable sum = 0
        for x = 0 to Array4D.length1 backing - 1 do
            for y = 0 to Array4D.length2 backing - 1 do
                for z = 0 to Array4D.length3 backing - 1 do
                    for w = 0 to Array4D.length4 backing - 1 do
                        sum <- sum + backing.[x, y, z, w]
        sum

let readFileToGrid (fn: string) : int[,] =
    let lines = Array.map (fun (x:String) -> x.Trim()) (System.IO.File.ReadAllLines(fn))
    Array2D.init (Array.length lines) (String.length lines.[0]) (fun x y -> if lines.[x].[y] = '#' then 1 else 0)

let simulate fn =
    let initialState = readFileToGrid fn
    let grid = SmartGrid initialState
    for i = 1 to CYCLES do
        grid.generateInPlace()
    grid.Count()

[<EntryPoint>]
let main argv =
    // test case
    assert ((simulate "input17_sample.txt") = 848)
    // main program execution
    let solution = simulate "input17.txt"
    printfn "Count after %d: %d" CYCLES solution 
    0