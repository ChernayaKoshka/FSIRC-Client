module TCP

open Extensions

open System
open System.Text
open System.Net.Sockets
open System.IO

type ConnectionDetails =
    {
        host : string
        port : int
    }

let connect (tcpClient : TcpClient) connectionDetails = async {
    return
        tcpClient.ConnectAsync(connectionDetails.host, connectionDetails.port)
        |> Async.awaitTaskResult
}

let stream (tcpClient : TcpClient) = tcpClient.GetStream()

let encodeStr (str : string) =
    Encoding.ASCII.GetBytes(str)

let decodeStr (bytes : byte[]) =
    Encoding.ASCII.GetString(bytes)

let read (tcpClient : TcpClient) = async {
    let stream = stream tcpClient

    let rec readNext data =
        if not stream.DataAvailable then
            data
        else
            let buffer = Array.create 2048 0uy
            let numRead = stream.Read(buffer, 0, buffer.Length)
            if numRead < 2048 then
                readNext (buffer.[0..numRead-1] :: data)
            else
                readNext (buffer :: data)

    return
        readNext []
        |> List.rev
        |> Array.concat
}

let write (tcpClient : TcpClient) data = async {
    let stream = stream tcpClient
    let res =
        stream.WriteAsync(data, 0, data.Length)
        |> Async.awaitTaskResult
    return res
}


