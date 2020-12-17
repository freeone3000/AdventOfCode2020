﻿open System

type State = { acc: int; pc: int }
type Insn = { name: string; arg: int }

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)
    
let parseInsn (raw:string) =
    let name = raw.Substring(0, 3)
    let value = match Int32.TryParse(raw.Substring(4)) with
                | true, x  -> x
                | false, _ -> 0
    { name = name; arg = value }

let evalStep insn state =
//    printfn "Visiting %s %+d" insn.name insn.arg
    match insn with
    | { name = "nop" } -> { acc = state.acc; pc = state.pc + 1 }
    | { name = "acc" } -> { acc = state.acc + insn.arg ; pc = state.pc + 1 }
    | { name = "jmp" } -> { acc = state.acc ; pc = state.pc + insn.arg }
    | _ -> failwith "Unhandled Insn"

let rec eval (program:Insn[]) (seen:Set<int>) state = // (terminated, state)
    if state.pc > program.Length then
        (false, state) // out of bounds
    else if state.pc = program.Length then
        (true, state) // actual only successful completion
    else if seen.Contains(state.pc) then
        (false, state) // infinite loop
    else
        let nextInsn = program.[state.pc]
        let nextState = evalStep nextInsn state
        eval program (Set.add state.pc seen) nextState

[<EntryPoint>]
let main argv =
    let program = readlines argv.[0]
                   |> Seq.map parseInsn
                   |> Seq.toArray
    let initState = { acc = 0 ; pc = 0 }
    let evalResult  = eval program Set.empty initState
    // TODO It's possible to change one Insn from nop to jmp, or jmp to nop, and have it complete.
    // Obvious solution is to try them all.
    // What's a less-obvious solution?
    match evalResult with
    | (_, finalState) -> printfn "Answer: %d" finalState.acc
    0