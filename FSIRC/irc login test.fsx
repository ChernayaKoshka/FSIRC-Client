#r @"..\packages\FParsec\lib\netstandard2.0\FParsecCS.dll"
#r @"..\packages\FParsec\lib\netstandard2.0\FParsec.dll"
#r @"..\packages\TaskBuilder.fs\lib\netstandard1.6\TaskBuilder.fs.dll"
#load "Types.fs"
#load "TCP.fs"
#load "Parsing.fs"
#load "IRC.fs"

open System.Net
open System.Net.Sockets
open FSIRC
open FSIRC.TCP
open System.Threading.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive

let cts = new System.Threading.CancellationTokenSource()
let t = task {
    let client = new TcpClient()
    do! connect client "localhost" 6667
    let stream = client.GetStream()
    let holding = IRC.startMessageLoop stream cts.Token
    let send = IRC.sendMessage stream
    do! IRC.login stream "My Real Name!" "WiZaRd" "WiZ"
    do! Task.Delay 5000
    do! IRC.Messages.join "#" None |> send
    do! Task.Delay 2000
    do! IRC.Messages.privMsg "#" "This is a test message!" |> send
    do! Task.Delay 2000
    do! IRC.Messages.part "#" (Some "Bye bye!") |> send
    do! Task.Delay 2000
    cts.Cancel()
}