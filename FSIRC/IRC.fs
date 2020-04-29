module FSIRC.IRC
open FParsec
open System
open System.Net.Sockets

type Command =
    | TextCommand of string
    | IntCommand of uint32
    with
        override this.ToString() =
            match this with
            | TextCommand str -> str
            | IntCommand int -> int.ToString()

type Message =
    {
        Prefix : string option
        Command : Command
        Parameters : string[]
    }
    with
        override this.ToString() =
            let prefix =
                match this.Prefix with
                | Some prefix ->
                    prefix + " "
                | None ->
                    String.Empty
            let command = this.Command.ToString()

            let args = String.concat " " this.Parameters

            sprintf "%s %s %s" prefix command args

module internal MessageParsing =
    type UserState =
        {
            ArgsParsed : int
        }
    type Parser<'t> = Parser<'t, UserState>

    let p3Digit : Parser<_> =
        puint32
        |>> (fun d3 ->
            if d3 <= 999u then
                d3
            else
                failwith "Err: > 999")

    let notNewline : Parser<_> =
        noneOf ['\r'; '\n']

    let notSpace : Parser<_> =
        noneOf [' ']

    let maxArgsExceeded (userState:UserState) : bool =
        userState.ArgsParsed >= 14

    let incrementArgCount : Parser<_> =
        updateUserState (fun state -> { state with ArgsParsed = state.ArgsParsed + 1 })

    let resetArgCount : Parser<_> =
        setUserState { ArgsParsed = 0 }

    let pPrefix : Parser<_> =
        pchar ':'
        >>. many1CharsTill anyChar (pchar ' ')

    let pCommandStr : Parser<_> =
        many1Chars asciiLetter
        |>> TextCommand

    let pCommandArg : Parser<_> =
        many1CharsTill (noneOf [ ' '; '\r'; '\n'; ':' ]) (anyOf [ ' '; '\r'; '\n'; ':' ])
        .>> incrementArgCount

    let pCommandArgs : Parser<_> =
        pipe2
            (manyTill pCommandArg (pchar ':' |>> ignore <|> userStateSatisfies maxArgsExceeded))
            (restOfLine false)
            (fun args final-> args @ [final])

    let pCommand : Parser<_> =
        pipe3
            (opt pPrefix)
            ((pCommandStr <|> (p3Digit |>> IntCommand)) .>> pchar ' ')
            pCommandArgs
            (fun prefix command args ->
                {
                    Prefix = prefix
                    Command = command
                    Parameters = Array.ofList args
                })
        .>> resetArgCount

    let ParseCommandStr str =
        match runParserOnString (many (pCommand .>> (many1 newline)))  { ArgsParsed = 0 } "" str with
        | Success (result,_,_) -> Result.Ok result
        | Failure (err,_,_) -> Result.Error err



    //let testData =
    //    [
    //        ":irc.foonet.com NOTICE * :*** Looking up your hostname...";
    //        ":irc.foonet.com NOTICE * :*** Found your hostname";
    //        "ERROR :Closing Link: [127.0.0.1] (Registration Timeout)"
    //    ]
    //    |> List.map test

open MessageParsing

let parseCommand = ParseCommandStr

let sendMessage (client : TcpClient) (message : Message) =
    ()


//let processMessage (message : Message) =

