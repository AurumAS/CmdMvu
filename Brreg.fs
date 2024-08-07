﻿module Brreg

open FSharp.Data

[<Literal>]
let private searchSample =
    "https://data.brreg.no/enhetsregisteret/api/enheter?navn=Sesam&fraRegistreringsdatoEnhetsregisteret=2010-10-20&tilRegistreringsdatoEnhetsregisteret=2023-01-09&konkurs=false"

let private searchUrl =
    "https://data.brreg.no/enhetsregisteret/api/enheter?forretningsadresse.landkode=NO&fraRegistreringsdatoEnhetsregisteret=2014-10-20&tilRegistreringsdatoEnhetsregisteret=2023-01-09&konkurs=false&navn="

type private BrregSearch = JsonProvider<searchSample>

let search searchTerm =
    let result = BrregSearch.Load(searchUrl + searchTerm)

    match result.Page.TotalElements with
    | 0 -> Array.empty
    | _ ->
        result.Embedded.Enheter
        |> Array.map (fun enhet ->
            {| OrgNr = enhet.Organisasjonsnummer
               Navn = enhet.Navn |})

open System.Text.Json
open System.Text.Json.Serialization

let private options = JsonSerializerOptions()
options.Converters.Add(JsonFSharpConverter())
options.WriteIndented <- true


[<Literal>]
let private enhetSample =
    "https://data.brreg.no/enhetsregisteret/api/enheter/914159350"

let private enhetsUrl = "https://data.brreg.no/enhetsregisteret/api/enheter/"

type private BrregEnhet = JsonProvider<enhetSample>

let companyDetails orgnr =
    let result = BrregEnhet.Load(enhetsUrl + orgnr)

    let adresse =
        {| Adresse = result.Forretningsadresse.Adresse
           Postnummer = result.Forretningsadresse.Postnummer
           Sted = result.Forretningsadresse.Poststed |}

    JsonSerializer.Serialize(
        {| OrgNr = result.Organisasjonsnummer
           Navn = result.Navn
           Forretningsadresse = adresse |},
        options
    )
