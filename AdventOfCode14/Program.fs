// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic
open System.Text.RegularExpressions

let mutable MEMORY = Dictionary<int, string>()

let applyMask (maskStr:string) (input:string): string =
    let mask (m:char) (i:char) =
        match m with
        | 'X' -> i
        | _ -> m // 0 or 1
    Seq.map2 mask maskStr input
    |> Seq.toArray
    |> String
    
let decimalTo36BitBinary (rawInput:int) =
    Convert.ToString(rawInput, 2).PadLeft(36, '0')

let weirdBinaryToNumber (rawOutput:string) =
    // inverse of decimalTo36BitBinary
    Convert.ToInt64(rawOutput, 2)
    
let (|Mask|_|) input =
    let parts = Regex.Split(input, "\s+=\s+")
    if parts.[0] = "mask" then
        Some parts.[1]
    else None
    
let (|Memory|_|) input =
    let regexMatch = Regex.Match(input, "mem\\[(\\d+)\\]\\s*=\\s*(\\d+)")
    if regexMatch.Success then
        let group (x:int) = regexMatch.Groups.[x].Captures.[0].Value // get around clunky syntax
        Some (group 1 |> int, group 2 |> int)
    else None
    
let applyMemoryChange mask (addr, value:int) =
        MEMORY.[addr] <- applyMask mask (decimalTo36BitBinary value)

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)

[<EntryPoint>]
let main argv =
    let mutable mask = "".PadLeft(36, 'X')
    
    let handleLine (line:string) =
        match line.Trim() with
        | Mask newMask -> (mask <- newMask)
        | Memory line -> applyMemoryChange mask line
        | "" -> ignore 1
        | _ -> raise (Exception("Could not handle line " + line))

    readlines argv.[0]
    |> Seq.iter handleLine
    
    // read the memory
    MEMORY.Values
    |> Seq.map weirdBinaryToNumber
    |> Seq.sum
    |> printf "Sum %d"
    
    0