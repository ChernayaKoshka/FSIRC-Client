[<RequireQualifiedAccess>]
module Helpers

open System
open System.Text.RegularExpressions
open FParsec
open Expecto
open Expecto.FParsec

open FSIRC.Parsing

let unwrap parseResult =
    match parseResult with
    | Success(res, _, _) ->
        res
    | Failure (err, _, _) ->
        failwithf "%A" err
        
let run parser =
    runParserOnString parser { ArgsParsed = 0 } ""

let runWithState (state : UserState) parser =
    runParserOnString parser state ""

let parseAndCompare parser data =
    fun () ->
        data
        |> List.iter (fun (input, expected) ->
            let res = run parser input
            sprintf "The parsing of '%s' did not succeed!" input
            |> Expect.isSuccess res

            let unwrapped = unwrap res
            sprintf "The parsed result of '%s' was not expected!" input
            |> Expect.equal unwrapped expected)

let parseAndExpectFailure parser data =
    fun () ->
        data
        |> List.iter (fun testStr ->
            let result = run parser testStr
            Expect.isFailure result "Should not have processed successfully"
        )