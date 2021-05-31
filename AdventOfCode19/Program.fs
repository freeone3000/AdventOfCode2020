open System.Collections.Generic
open System.Text.RegularExpressions

///
/// Definitions of rules
///

type Rule = string -> (bool * int) // match, number of characters removed
let IsRule (str: string) = str.Contains(":")
let RuleOr (a: Rule) (b: Rule) =
    fun ch ->
        let lhs, count = a(ch)
        if lhs then true, count
        else
            let rhs, count = b(ch)
            if rhs then true, count
            else false, 0

let mutable ALL_RULES = Dictionary<int, Rule>()

///
/// Internal matching
///

let (|Literal|_|) (input:string) =
    if input.StartsWith("\"") then
        Some (input.Substring(1, input.Length - 2))
    else None

let (|OrPatterns|_|) (input:string) =
    let parts = Regex.Split(input, "\\s+\\|\\s+")
    if parts.Length > 1 then
        Some parts
    else None

let rec computeBasedOn (rules: seq<Rule>) (str:string) =
    let foldInMatch (matchSoFar: bool, curIndex: int) (rule: Rule) =
        if matchSoFar then
            match rule(str.Substring(curIndex)) with
            | (true, amountTaken) -> (true, curIndex + amountTaken)
            | (false, _) -> (false, curIndex)
        else (false, curIndex)
    
    rules
    |> Seq.fold foldInMatch (true, 0)
    
let parseRefList ruleString =
    let rules = Regex.Split(ruleString, "\\s+")
                |> Seq.map (int >> (fun ruleRef -> ALL_RULES.[ruleRef]))
    computeBasedOn rules // partial application

let rec parseInnerRule(ruleString) =
    // we use active patterns here for ease of reading
    match ruleString with
    | Literal x -> (fun (ch:string) -> ch.Substring(0, x.Length).Equals(x), x.Length)
    | OrPatterns patterns -> patterns
                             |> Seq.map parseInnerRule
                             |> Seq.reduce RuleOr
    | _ -> parseRefList ruleString

///
/// Entry points
///

let parseRule (line:string) =
    // parse out the rule number from the rule
    let parts = Regex.Split(line, ":\\s+")
    let idx = parts.[0] |> int
    ALL_RULES.[idx] <- parseInnerRule(parts.[1])

let readlines (fn:string) : seq<string> =
    System.IO.File.ReadLines(fn)

[<EntryPoint>]
let main argv =
    let fn = argv.[0]
    let lines = readlines fn
    
    // parse rules
    lines
    |> Seq.filter IsRule
    |> Seq.iter parseRule
    
    // parse input
    let toMatch = lines
                       |> Seq.filter (not << IsRule)
                       |> Seq.filter (fun x -> x.Trim().Length > 0)
    let matchedLines = toMatch
                       |> Seq.map (fun x -> x, ALL_RULES.[0](x))
                       |> Seq.filter(fun (source, (x, count)) -> x && (source.Length = count)) // length check to ensure complete matches
                       
    printfn ("Matched rules: %d") (Seq.length matchedLines)
    
    printfn ("\nActual matches:\n")
    Seq.iter (fst >> (printfn "Match: %s")) matchedLines
    
    0
    