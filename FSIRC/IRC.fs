module FSIRC.IRC
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

let pLetter : Parser<_> =
    anyOf (['A'..'Z'] @ ['a'..'z'])

let pDigit : Parser<_> =
    anyOf ['0'..'9']

let pShortName : Parser<_> =
    sepBy (pDigit <|> pLetter) (pchar '-')
    |>> (Array.ofList >> String)

