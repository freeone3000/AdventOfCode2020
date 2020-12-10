open System
open System.Collections.Generic
open System.Text.RegularExpressions

type PassportData = Dictionary<string, string>

let readlines fn : seq<string> =
    System.IO.File.ReadLines(fn)

let dataRegex = Regex(@"(\w{3}):(\S+)")

let mergePassportData (passport:PassportData) line =
    for m in dataRegex.Matches(line) do
        let name = m.Groups.Item(1).Value
        let value = m.Groups.Item(2).Value
        passport.[name] <- value

let processPassports (lines:seq<string>) : list<PassportData> =
    let mutable passports = []
    // rewriting this in functional terms is left as an exercise for the reader
    let mutable curPassport = PassportData()
    for line in lines do
        if line.Length = 0 then
            passports <- curPassport :: passports
            curPassport <- PassportData()
        else
            mergePassportData curPassport line
    passports <- curPassport :: passports
    passports
    
let isValid passport =
    let isSafe (passport:PassportData) field =
        passport.GetValueOrDefault(field, "") <> ""
    
    let fields = seq { "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" } // no cid
    Seq.forall (isSafe passport) fields
    
[<EntryPoint>]
let main argv =
    let valid = processPassports (readlines argv.[0])
                |> List.filter isValid
                |> List.length
    printfn "Valid:\n%d" valid
    0