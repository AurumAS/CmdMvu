module CmdMvu.SpectreFSharp.Host

open Spectre.Console.FSharp

type OrgNr = OrgNr of string
module OrgNr = 
    let value (OrgNr value) = value

type SearchCompanyResult = { Name: string; OrgNr: OrgNr }

type SearchCompanyResults = 
    {
        SearchTerm: string
        SearchResult: SearchCompanyResult seq
    }

type Model = 
    | StartScreen
    | SearchCompany
    | NoCompanyFound of string
    | SearchCompanyResult of SearchCompanyResults
    | CompanyDetails of SearchCompanyResults * string
    | Exit

type Message = 
    | ShowSearch
    | SearchForCompany of string
    | ShowCompanyDetails of OrgNr
    | BackToSearchResult 
    | ExitScreen 
    | Exited


let view model = 
    match model with
    | StartScreen -> 
        let selectionOptions = [ "Search for company"; "Exit"] |> List.map Choice
        console {
            figlet {
                text "Company searcher 2000"
                aligned Left
            }
            let choice = selection {
                title "Choose action"
                choices selectionOptions
            } 
            return match choice with
                | Choice "Exit" -> Message.ExitScreen
                | Choice _ -> Message.ShowSearch
        }
    | SearchCompany ->
        console {
            let validator = Some <| (fun (answer:string) -> 
                if answer.Length < 3 then
                    Spectre.Console.ValidationResult.Error("Search must be at least 3 characters")
                else
                    Spectre.Console.ValidationResult.Success()) 

            let searchTerm = textPrompt {
                question "Search for company name: "
                validation validator
            }
            return searchTerm |> SearchForCompany 
        }
    | NoCompanyFound searchTerm -> 
        console {
            error {
                description $"No company found for search term {searchTerm}"
            }
            return ShowSearch
        }
    | SearchCompanyResult searchresults -> 
        let selectionOptions = (searchresults.SearchResult |> Seq.map (fun result -> $"{result.OrgNr |> OrgNr.value} {result.Name}" |> Choice ) |> Seq.toList) @ ["New search" |> Choice; "Exit" |> Choice]
        console {
            let choice = selection  {
                title "Search results, select company to view details"
                choices selectionOptions 
            }
            return match choice with 
                | Choice "New search" -> ShowSearch
                | Choice "Exit" -> ExitScreen
                | Choice companyname -> 
                    let orgnr = companyname.[0..9]
                    orgnr |> OrgNr |> Message.ShowCompanyDetails        
        }
    | CompanyDetails (searchResult, jsonstring) ->
        let selecitonOptions = [ "Back"; "New Search"; "Exit"] |> List.map Choice
        console {
            jsonview {
                json jsonstring
            }
            let choice = selection {
                title "What now?"
                choices selecitonOptions
            }
            return match choice with
            | Choice "Back" -> BackToSearchResult
            | Choice "New Search" -> ShowSearch
            | Choice "Exit" -> ExitScreen
            | _ -> failwith "Invalid choice"
        }
    | Exit ->
        console {
            figlet {
                text "K. Thanks. Bye"
                aligned Left
            }
            return Exited
        }

let update model message = 
    match message with 
    | ShowSearch -> Model.SearchCompany |> Some
    | SearchForCompany searchTerm -> 
        let searchResult = 
            searchTerm 
            |> Brreg.search
            |> Array.truncate 10
            |> Array.map (fun r -> { Name = r.Navn; OrgNr = r.OrgNr |> string |> OrgNr })
        match searchResult.Length with
        | 0 -> searchTerm |> NoCompanyFound |> Some
        | _ -> { SearchTerm = searchTerm; SearchResult = searchResult } |> SearchCompanyResult |> Some
    | BackToSearchResult ->
        match model with
        | CompanyDetails (searchResult, _) -> searchResult |> Model.SearchCompanyResult |> Some
        | _ -> failwith "Invalid state transition"    
    | ShowCompanyDetails (OrgNr orgnr) ->
        match model with
        | SearchCompanyResult searchResult -> 
            let companyDetails = orgnr |> Brreg.companyDetails
            (searchResult, companyDetails ) |> CompanyDetails |> Some
        | _ -> failwith "Invalid state transition"
    | ExitScreen -> Model.Exit |> Some
    | Exited -> None
        

[<EntryPoint>]
let main args =
    
   let rec mvuloop model =
        match model |> view |> update model with
        | None -> ()
        | Some newModel -> newModel |> mvuloop  
        
   mvuloop StartScreen
   System.Console.ReadLine() |> ignore
   0