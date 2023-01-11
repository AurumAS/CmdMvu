module Spectre.Console.FSharp

open Spectre.Console 
open Spectre.Console.Json
open Spectre.Console.Rendering

type Alignement = | Left | Center | Right

type FigletConfig = 
     {
         Aligned: Alignement
         Color: Spectre.Console.Color
         Text: string
     }

type FigletBuilder() = 
     member _.Yield _ = 
         {
             Text = ""
             Color = Spectre.Console.Color.Red
             Aligned = Left
         }

     member _.Run(config) = 
        
        let text = new FigletText(config.Text)
        match config.Aligned with
            | Left -> text.Justification <- Justify.Left
            | Center -> text.Justification <- Justify.Center
            | Right -> text.Justification <- Justify.Right
        text.Color <- config.Color
        text |> AnsiConsole.Write
        ()

     [<CustomOperation "text">]
     member _.Text(state, text) = { state with Text = text }
 
     [<CustomOperation "color">]
     member _.Color(state, color) = { state with Color = color}

     [<CustomOperation "aligned">]
     member _.Aligned(state, alignement) = { state with Aligned = alignement}


type Choice = Choice of string

type SelectionConfig = 
    {
        Title: string
        PageSize: int
        MoreChoicesText: string
        Choices: Choice list
    }


type SelectionBuilder() =
    member _.Yield(_) = 
        {
            Title = ""
            PageSize = 5
            MoreChoicesText = "More"
            Choices = list.Empty
        }
    member _.Run(config) = 
        let prompt = SelectionPrompt<string>()
        prompt.Title <- config.Title
        prompt.PageSize <- config.PageSize
        prompt.MoreChoicesText <- config.MoreChoicesText 
        prompt.AddChoices(config.Choices |> Seq.map (fun (Choice choice) -> choice)) |> ignore
        prompt |> AnsiConsole.Prompt |> Choice

    [<CustomOperation("title")>]
    member _.Title(state, title) = { state with Title = title }

    [<CustomOperation("morceChoicesText")>]
    member _.MorceChoices(state, morechoicestext) = { state with MoreChoicesText = morechoicestext }

    [<CustomOperation("pageSize")>]
    member _.PageSize(state, pageSize) = { state with PageSize = pageSize }

    [<CustomOperation("choices")>]
    member _.Choices(state, choices) = { state with Choices = choices }

type TextPromptConfig = 
    {
        Question: string
        Validation: (string -> ValidationResult) option
    }
type TextPromptBuilder() = 
    member _.Yield(_) = 
        {
            Question = ""
            Validation = None
        }
    member _.Run(config) =
        let prompt = new TextPrompt<string>(config.Question)
        match config.Validation with
        | None -> ()
        | Some validation -> 
            prompt.Validator <- validation
        prompt |> AnsiConsole.Prompt<string> 

    [<CustomOperation("question")>]
    member _.Question(state, question) = { state with Question = question }

    [<CustomOperation("validation")>]
    member _.Validation(state, validation) = { state with Validation = validation}
type ErrorConfig = 
    {
        Description: string
    }
type ErrorBuilder() =
    member _.Yield(_) = 
        {
            Description = ""
        }
    member _.Run(config) =
        AnsiConsole.Write(new Markup($"[red]{config.Description}[/]") :> Renderable)
        AnsiConsole.WriteLine()
        ()
    [<CustomOperation("description")>]
    member _.Description(state, error) = { state with Description = error}

type JsonViewConfig = 
    {
        Json: string
        Header: string
    }
type JsonViewBuilder() = 
    member _.Yield(_) = 
        {
            Json = ""
            Header = ""
        }
    member _.Run(config: JsonViewConfig) =
        let jsonText = new JsonText(config.Json)
        let panel = new Panel(jsonText)
        panel.Header <- new PanelHeader(config.Header)
        panel.RoundedBorder().Collapse().BorderColor(Color.Yellow) |> AnsiConsole.Write
        ()
    
    [<CustomOperation("json")>]
    member _.Json(state, json) = { state with Json = json }
    
    [<CustomOperation("header")>]
    member _.Header(state, header) = { state with Header = header }

type AnsiConsole() = 
    
    member _.Return(x) = 
        x
let error = ErrorBuilder()
let jsonview = JsonViewBuilder()
let textPrompt = TextPromptBuilder()
let selection = SelectionBuilder()
let figlet = FigletBuilder()
let console = AnsiConsole()
