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
    
let isValid (passport:PassportData) =
    let safeint (str:string):int =
        match Int32.TryParse(str) with
        | true, x -> x
        | _ -> -1

    let isHex (str:string) =
        seq { for c in str.Substring(1) -> c }
        |> Seq.forall (fun ch -> (ch >= 'a' && ch <= 'f') || (ch >= '0' && ch <= '9'))
    
    let validEyeColor str =
        let validColors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
        validColors |> List.exists ((=) str)
    
    let validHeight (str:string) =
        if str.EndsWith("cm") && str.Length >= 5 then
            let hgt = safeint (str.Substring(0, 3))
            hgt >= 150 && hgt <= 193
        else if str.EndsWith("in") && str.Length >= 4 then
            let hgt = safeint (str.Substring(0, 2))
            hgt >= 59 && hgt <= 76
        else
            false
    
    let validators = seq {
        "byr", safeint >> (fun x -> x >= 1920 && x <= 2002)
        "iyr", safeint >> (fun x -> x >= 2010 && x <= 2020)
        "eyr", safeint >> (fun x -> x >= 2020 && x <= 2030)
        "hgt", validHeight
        "hcl", (fun (x:string) -> x.Length = 7 && x.[0] = '#' && isHex x)
        "ecl", validEyeColor
        "pid", (fun (x:string) -> x.Length = 9 && (safeint x) >= 0)
    }

    validators
    |> Seq.forall (fun (field, validator) ->
        match passport.TryGetValue(field) with // if the field is present, validate, else, false
        | true, value -> (validator value)
        | _ -> false
    )
    
[<EntryPoint>]
let main argv =
    let valid = processPassports (readlines argv.[0])
                |> List.filter isValid
                |> List.length
    printfn "Valid:\n%d" valid
    0