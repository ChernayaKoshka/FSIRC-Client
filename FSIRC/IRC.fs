module FSIRC.IRC

open FSIRC.TCP
open FSIRC.Parsing

open System
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Net.Sockets
open System.Threading.Tasks
open System.Threading

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

let parseMessage = decodeStr >> (parse pMessage)

let processMessages (stream: NetworkStream) (messages: byte array array) = task {
    let parsed = Array.map parseMessage messages

    for message in parsed do
        match message with
        | Ok message ->
            do! handleMessage stream message
        | Error err ->
            Console.WriteLine(sprintf "%A" err)
    }

let getMessageParts (bytes : byte array) =
    let rec next accumulator remaining =
        match remaining |> Array.tryFindIndex ((=) (byte '\n')) with
        | Some index when remaining.[index - 1] = byte '\r' ->
            next (remaining.[..index] :: accumulator) remaining.[index+1..]
        | _ -> (accumulator, remaining)
    let (messages, remaining) = next List.empty bytes
    (List.rev messages, remaining)

let processReceived bytes =
    let (messages, remaining) = getMessageParts bytes
    let newBuffer = Array.zeroCreate bytes.Length
    Array.blit remaining 0 newBuffer 0 (Array.length remaining)
    (Array.ofList messages, newBuffer)

type IRCConnection(details : ConnectionDetails) =
    let client = new TcpClient()
    let mutable stream : NetworkStream option = None

    interface IDisposable with
        member _.Dispose() = client.Dispose()

    with
        member __.Login realName user nick = task {
            let nickCommand = { Prefix = None; Command = TextCommand "NICK"; Params = { Middle = [ nick ]; Trailing = None } }
            let userCommand = { Prefix = None; Command = TextCommand "USER"; Params = { Middle = [ user; "0"; "*" ]; Trailing = Some realName } }
            do! sendMessage stream.Value nickCommand
            do! sendMessage stream.Value userCommand
        }

        member __.BeginProcessingReceived (token : CancellationToken) = task {
            do! connect client details

            stream <- client.GetStream() |> Some

            Console.WriteLine("Hello!")
            let mutable buffer : byte array = Array.zeroCreate 4096
            while not token.IsCancellationRequested do
                try
                    let! _ = stream.Value.ReadAsync(buffer, buffer |> Array.findIndex ((=) 0uy), buffer.Length, token)
                    let (messages, newBuffer) = processReceived buffer
                    do! processMessages stream.Value messages
                    buffer <- newBuffer
                with
                | :? TaskCanceledException -> ()
            Console.WriteLine("Goodbye!")
        }

