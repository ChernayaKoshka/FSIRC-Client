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

type NickName = string
type HostName = string
type ServerName = HostName
type HostAddress = IPAddress

type Host =
    | HostName of HostName
    | HostAddress of HostAddress

type User = string

type UserInfo =
    {
        NickName: NickName
        User: User option
        Host: Host option
    }

type Prefix =
    | ServerName of ServerName
    | User of UserInfo
    // ambiguous case
    | NickNameOrServerName of string

type Params =
    {
        Middle: string list
        Trailing: string option
    }

type Message =
    {
        Prefix : Prefix option
        Command : Command
        Params : Params
    }
    with
        override this.ToString() =
            let prefix = string this.Prefix
            let command = string this.Command

            let args = (String.concat " " this.Params.Middle) + (match this.Params.Trailing with Some trailing -> " :" + trailing | None -> String.Empty)

            sprintf "%s %s %s" prefix command args

type Channel =
    {
        Prefix : string
        Name : string
        Postfix : string option
    }

type UserTarget =
    {
        User: User
        Host: Host option
        ServerName: ServerName option
    }

// TODO: Should be a DU of whatver $/# mean
type TargetMask = char

type MsgTo =
    | Channel of Channel
    | User of UserTarget
    | TargetMask of TargetMask
    | ServerUser of UserInfo