open System.Threading
#load @"..\references.fsx"

open Extensions
open TCP
open IRC
open System
open System.Text
open System.Net.Sockets

let client = new TcpClient()

//let connectResult =
//    //connect client { host = "motherfuckingwebsite.com"; port = 80 }
//    connect client { host = "localhost"; port = 6697 }
//    |> Async.RunSynchronously

//let testData =
//    Encoding.ASCII.GetBytes("GET / HTTP/1.1\r\nHost: motherfuckingwebsite.com\r\nUser-Agent: benis/1.0\r\nAccept: */*\r\n\r\n")

let reader = async {
    printfn "READER THREADID: %d" Thread.CurrentThread.ManagedThreadId
    Console.WriteLine("Polling!")
    while true do
        let stream = stream client
        while not stream.DataAvailable do
            do! Async.Sleep 10
        printfn "Avail?: %b" stream.DataAvailable
        let! data = read client
        let text = decodeStr data
        Console.WriteLine(sprintf "RECEIVED: |%A|%s|" data (text.Trim()))
        //Console.WriteLine(sprintf "%A" <| parseCommand text)
}

let passMsg = encodeStr "PASS secretpasswordhere\r\n"
let nickMsg = encodeStr "NICK Wiz\r\n"
let userMsg = encodeStr "USER guest 0 * :Ronnie Reagan\r\n"

let login = async {
    printfn "LOGIN THREADID: %d" Thread.CurrentThread.ManagedThreadId
    Console.WriteLine("Starting Login!")
    while not client.Connected do
        Console.WriteLine("Client not connected?")
        do! Async.Sleep 20

    Console.WriteLine("Sending password...")
    let! a = write client passMsg
    Console.WriteLine("Sending nick...")
    let! b = write client nickMsg
    Console.WriteLine("Sending user...")
    let! c = write client userMsg
    printfn "\n\n\n\nRESULT1:%A\n\n\n\nRESULT2:%A\n\n\n\nRESULT3:%A\n\n\n\n" a b c
    return ()
}

printfn "TEST THREADID: %d" Thread.CurrentThread.ManagedThreadId
connect client { host = "65.49.60.64"; port = 6667 }
|> Async.RunSynchronously

while not client.Connected do
    Console.WriteLine("Not connected yet!")
    System.Threading.Thread.Sleep(100)

reader
|> Async.Start

login
|> Async.RunSynchronously