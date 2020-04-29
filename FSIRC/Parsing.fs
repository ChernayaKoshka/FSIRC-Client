module FSIRC.Parsing
open FParsec
open System
open System.Net.Sockets

type UserState =
    {
        ArgsParsed : int
    }
type Parser<'t> = Parser<'t, UserState>

let maxArgsExceeded (userState:UserState) : bool =
    userState.ArgsParsed >= 14

let incrementArgCount : Parser<_> =
    updateUserState (fun state -> { state with ArgsParsed = state.ArgsParsed + 1 })

let resetArgCount : Parser<_> =
    setUserState { ArgsParsed = 0 }

let letter =
    (['A'..'Z'] @ ['a'..'z'])

let digit =
    ['0'..'9']

let pLetter : Parser<_> = anyOf letter
let pDigit : Parser<_> = anyOf digit
let pLetterOrDigit : Parser<_> = anyOf (digit @ letter)

let pShortName : Parser<_> =
    (*
        abc
        abc-123
        abc-def-123

        abc-
        -abc
    *)
    pLetterOrDigit
    .>>. sepBy (many1Chars pLetterOrDigit) (pchar '-')
    |>> (fun (first, rest) ->
        string first + (String.concat "-" rest))


