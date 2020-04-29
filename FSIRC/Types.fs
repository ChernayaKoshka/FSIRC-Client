[<AutoOpen>]
module FSIRC.Types

open System
open System.Net

type Command =
    | TextCommand of string
    | IntCommand of uint32
    with
        override this.ToString() =
            match this with
            | TextCommand str -> str
            | IntCommand int -> int.ToString()

type HostName = string
type ServerName = HostName
type HostAddress = IPAddress

type Host =
    | HostName of HostName
    | HostAddress of HostAddress

type User = string

type UserInfo =
    {
        NickName: string
        User: User option
        Host: Host option
    }

type Prefix =
    | ServerName of ServerName
    | User of UserInfo

type Message =
    {
        Prefix : Prefix option
        Command : Command
        Parameters : string[]
    }
    with
        override this.ToString() =
            let prefix = string this.Prefix
            let command = string this.Command

            let args = String.concat " " this.Parameters

            sprintf "%s %s %s" prefix command args