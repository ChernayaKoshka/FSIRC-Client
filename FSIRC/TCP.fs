module FSIRC.TCP

open System
open System.Text
open System.Net.Sockets
open System.IO
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Threading.Tasks

type ConnectionDetails =
    {
        host : string
        port : int
    }

let connect (tcpClient : TcpClient) connectionDetails = task {
    return!
        tcpClient.ConnectAsync(connectionDetails.host, connectionDetails.port)
}

let getStream (tcpClient : TcpClient) = tcpClient.GetStream()

let encodeStr (str : string) =
    Encoding.ASCII.GetBytes(str)

let decodeStr (bytes : byte[]) =
    Encoding.ASCII.GetString(bytes)

let write (stream: NetworkStream) data = task {
    return! stream.WriteAsync(data, 0, data.Length)
}