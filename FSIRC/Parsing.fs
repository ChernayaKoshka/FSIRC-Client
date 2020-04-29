module FSIRC.Parsing
open FParsec
open System
open System.Net.Sockets
open System.Net

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

let hexDigit = ['A'..'F'] @ ['a'..'f'] @ digit

let pLetter : Parser<_> = anyOf letter
let pDigit : Parser<_> = anyOf digit
let pLetterOrDigit : Parser<_> = anyOf (digit @ letter)
let pHexDigit = anyOf hexDigit

let pShortName : Parser<_> =
    (*
        abc
        abc-123
        abc-def-123

        abc-
        -abc
    *)
    pLetterOrDigit
    .>>. stringsSepBy (many1Chars pLetterOrDigit) (pstring "-")
    |>> (fun (start, rest) -> string start + rest)

let pHostName : Parser<_> =
    stringsSepBy1 pShortName (pstring ".")

// could potentially allow for malformed IPV4 addresses, but this conforms to the IRC spec... hm...
let pIPv4 : Parser<_> =
    let part : Parser<string> = manyMinMaxSatisfy 1 3 Char.IsDigit
    pipe4
        (part .>> pchar '.')
        (part .>> pchar '.')
        (part .>> pchar '.')
        part
        (fun a b c d -> sprintf "%s.%s.%s.%s" a b c d |> IPAddress.Parse)

let pIPv6 : Parser<_> =
    let pTypical : Parser<_> =
        let part : Parser<string> = many1Chars pHexDigit
        part                 >>= fun a ->
        (pchar ':' >>. part) >>= fun b ->
        (pchar ':' >>. part) >>= fun c ->
        (pchar ':' >>. part) >>= fun d ->
        (pchar ':' >>. part) >>= fun e ->
        (pchar ':' >>. part) >>= fun f ->
        (pchar ':' >>. part) >>= fun g ->
        (pchar ':' >>. part) >>= fun h -> preturn (sprintf "%s:%s:%s:%s:%s:%s:%s:%s" a b c d e f g h |> IPAddress.Parse)
    let pIPv4Format : Parser<_> =
        pipe4
            (pstring "0:0:0:0:0:")
            (pstring "0" <|> pstring "FFFF")
            (pstring ":")
            pIPv4
            (fun a b c address -> IPAddress.Parse(a + b + c + string address))
            // TODO: ^^^ Absolutely nasty, I need a better way of doing this.
            // Maybe refactor IPV6/IPV4 to return string instead of address?
    pIPv4Format <|> pTypical