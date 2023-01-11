module Brreg

open FSharp.Data

[<Literal>]
let searchSample = "https://data.brreg.no/enhetsregisteret/api/enheter?navn=Sesam&fraRegistreringsdatoEnhetsregisteret=2010-10-20&tilRegistreringsdatoEnhetsregisteret=2023-01-09&konkurs=false"
let searchUrl = "https://data.brreg.no/enhetsregisteret/api/enheter?forretningsadresse.landkode=NO&fraRegistreringsdatoEnhetsregisteret=2014-10-20&tilRegistreringsdatoEnhetsregisteret=2023-01-09&konkurs=false&navn="

type BregSearch = JsonProvider<searchSample>



let search searchTerm = 
    let result = BregSearch.Load(searchUrl + searchTerm)
    match result.Page.TotalElements with
    | 0 -> Array.empty
    | _ ->  result.Embedded.Enheter 
            |> Array.map (fun enhet -> {| OrgNr = enhet.Organisasjonsnummer; Navn = enhet.Navn |} )
    

open System.Text.Json
open System.Text.Json.Serialization

let options = JsonSerializerOptions()
options.Converters.Add(JsonFSharpConverter())
options.WriteIndented <- true


[<Literal>]
let enhetSample = "https://data.brreg.no/enhetsregisteret/api/enheter/914159350"
let enhetsUrl = "https://data.brreg.no/enhetsregisteret/api/enheter/"
type BregEnhet = JsonProvider<enhetSample>

let companyDetails orgnr = 
    let result = BregEnhet.Load(enhetsUrl + orgnr)
    
    let adresse = {| Adresse = result.Forretningsadresse.Adresse; Postnummer = result.Forretningsadresse.Postnummer; Sted = result.Forretningsadresse.Poststed; |}
    JsonSerializer.Serialize ({| OrgNr = result.Organisasjonsnummer; Navn = result.Navn; Forretningsadresse = adresse; |}, options)
    