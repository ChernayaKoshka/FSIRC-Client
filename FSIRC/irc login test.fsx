#r @"..\packages\FParsec\lib\netstandard2.0\FParsecCS.dll"
#r @"..\packages\FParsec\lib\netstandard2.0\FParsec.dll"
#r @"..\packages\TaskBuilder.fs\lib\net45\TaskBuilder.fs.dll"
#load "Types.fs"
#load "TCP.fs"
#load "Parsing.fs"
#load "IRC.fs"
#load "FSIRC.fs"

open FSIRC.IRC
open FSIRC.TCP
open System.Threading.Tasks

let client = new IRCConnection({ host = "localhost"; port = 6667 })

let cts = new System.Threading.CancellationTokenSource()

client.BeginProcessingReceived cts.Token

client.Login "My Real Name!" "WiZaRd" "WiZ"