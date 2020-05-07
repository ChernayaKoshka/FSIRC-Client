module FSIRC.TCP

open System.Net.Sockets
open System.Net
open System.Text
open FSharp.Control.Tasks.V2.ContextInsensitive

let connect (tcpClient : TcpClient) (host: string) port = task {
    return! tcpClient.ConnectAsync(host, port)
}

let directConnect (tcpClient : TcpClient) (host: IPAddress) port = task {
    return! tcpClient.ConnectAsync(host, port)
}

let encodeStr (str : string) =
    Encoding.ASCII.GetBytes(str)

let decodeStr (bytes : byte[]) =
    Encoding.ASCII.GetString(bytes)

let write (stream: NetworkStream) data = task {
    return! stream.WriteAsync(data, 0, data.Length)
}