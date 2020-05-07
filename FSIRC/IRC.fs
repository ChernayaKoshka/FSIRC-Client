[<RequireQualifiedAccess>]
module FSIRC.IRC

open FSIRC.TCP
open FSIRC.Parsing

open System
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Net.Sockets
open System.Threading.Tasks
open System.Threading

[<AutoOpen>]
module Messages =
    let nick nick = { Prefix = None; Command = TextCommand "NICK"; Params = { Middle = [ nick ]; Trailing = None } }

    let user user realName = { Prefix = None; Command = TextCommand "USER"; Params = { Middle = [ user; "0"; "*" ]; Trailing = Some realName } }

    let privMsg destination contents = { Prefix = None; Command = TextCommand "PRIVMSG"; Params = { Middle = [ destination ]; Trailing = Some contents } }

    let oper name password = { Prefix = None; Command = TextCommand "OPER"; Params = { Middle = [ name; password ]; Trailing = None } }

    let mode nickname modes = { Prefix = None; Command = TextCommand "MODE"; Params = { Middle = [ nickname; modes ]; Trailing = None } }

    let quit quitMessage = { Prefix = None; Command = TextCommand "QUIT"; Params = { Middle = List.empty; Trailing = quitMessage } }

    let join channel key =
        match key with
        | Some key -> { Prefix = None; Command = TextCommand "JOIN"; Params = { Middle = [ channel; key ]; Trailing = None } }
        | None -> { Prefix = None; Command = TextCommand "JOIN"; Params = { Middle = [ channel ]; Trailing = None } }

    let part channel partMessage = { Prefix = None; Command = TextCommand "PART"; Params = { Middle = [ channel ]; Trailing = partMessage } }

let sendMessage (stream: NetworkStream) (message: Message) = task {
    Console.WriteLine(sprintf "Sending: \"%s\"" (string message))
    let toSend =
        message
        |> string
        |> encodeStr
    do! write stream toSend
}

let handleMessage (stream: NetworkStream) (message: Message) = task {
    match message.Command with
    | IntCommand ServerReply.RPL_WELCOME ->
        Console.WriteLine("Success!!!!")
    | TextCommand "NOTICE" | TextCommand "ERROR" ->
        Console.WriteLine(string message)
    | TextCommand "PING" ->
        let toSend =
            { message with
                Prefix = None
                Command = TextCommand "PONG"
            }
        do! sendMessage stream toSend
    | other ->
        Console.WriteLine(sprintf "Wanted to process %A but didn't know how" other)
}

let processMessages (stream: NetworkStream) (messages: byte array array) = task {
    let parsed =
        messages
        |> Array.map (fun message ->
            message
            |> decodeStr
            |> parse pMessage)

    for message in parsed do
        match message with
        | Ok message ->
            do! handleMessage stream message
        | Error err ->
            Console.WriteLine(sprintf "%A" err)
    }

let getMessageChunksFromBytes bytes =
    let getMessageParts (bytes : byte array) =
        let rec next accumulator remaining =
            match remaining |> Array.tryFindIndex ((=) (byte '\n')) with
            | Some index when remaining.[index - 1] = byte '\r' ->
                next (remaining.[..index] :: accumulator) remaining.[index+1..]
            | _ -> (accumulator, remaining)
        let (messages, remaining) = next List.empty bytes
        (List.rev messages, remaining)

    let (messages, remaining) = getMessageParts bytes
    let newBuffer = Array.zeroCreate bytes.Length
    Array.blit remaining 0 newBuffer 0 (Array.length remaining)
    (Array.ofList messages, newBuffer)

let login stream realName userName nickName = task {
    do! nick nickName |> sendMessage stream
    do! user userName realName |> sendMessage stream
}

let startMessageLoop (stream: NetworkStream) (token: CancellationToken) = task {
    // https://stackoverflow.com/a/12893018/2396111
    use _ = token.Register(fun _ -> stream.Close())

    Console.WriteLine("Hello!")
    let mutable buffer : byte array = Array.zeroCreate 4096
    while not token.IsCancellationRequested do
        try
            let bufIndex = buffer |> Array.findIndex ((=) 0uy)
            let! _ = stream.ReadAsync(buffer, bufIndex, buffer.Length - bufIndex, token)
            let (messages, newBuffer) = getMessageChunksFromBytes buffer
            do! processMessages stream messages
            buffer <- newBuffer
        with
        | :? OperationCanceledException -> ()
        | ex -> printfn "%A" ex
    Console.WriteLine("Goodbye!")
}
