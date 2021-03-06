module FSIRC.Parsing
open FParsec
open System
open System.Net.Sockets
open System.Net

type UserState =
    {
        ArgsParsed : int
    }
type Parser<'t> = Parser<'t, UserState>

let maxArgsNotReached (userState:UserState) : bool =
    userState.ArgsParsed <= 14

let maxArgsReached (userState:UserState) : bool =
    userState.ArgsParsed = 14

let incrementArgCount : Parser<_> =
    updateUserState (fun state -> { state with ArgsParsed = state.ArgsParsed + 1 })

let resetArgCount : Parser<_> =
    setUserState { ArgsParsed = 0 }

let upperLetter = ['A'..'Z']
let lowerLetter = ['a'..'z']

let letter = upperLetter @ lowerLetter

let digit = ['0'..'9']

let hexDigit = ['A'..'F'] @ ['a'..'f'] @ digit

let special = ['['..'`'] @ ['{'..'}']

let noEsc = [ '\x00'; '\\' ]

// letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
let pLetter : Parser<_> = anyOf letter

// digit      =  %x30-39                 ; 0-9
let pDigit : Parser<_> = anyOf digit
let pLetterOrDigit : Parser<_> = anyOf (digit @ letter)

// hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
let pHexDigit = anyOf hexDigit

// special    =  %x5B-60 / %x7B-7D
//                 ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
let pSpecial : Parser<_> = anyOf special

// nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
// ; any octet except NUL, CR, LF, " " and ":"
let pNoSpcCrLfCl : Parser<_> = noneOf [ '\x00'; '\r'; '\n'; ' '; ':'  ]

// SPACE      =  %x20        ; space character
let pSpace : Parser<_> = pchar ' '

// https://bitbucket.org/fparsec/main/issues/18/cant-create-a-parser-to-parse-r-n-exactly
// crlf       =  %x0D %x0A   ; "carriage return" "linefeed"
let pCrLf : Parser<_> =
    let error = expected "newline (\\r\\n)"
    let crcn = TwoChars('\r', '\n')
    fun stream ->
        if stream.Skip(crcn) then
            stream.RegisterNewline()
            Reply(())
        else
            Reply(Error, error)

// shortname  =  ( letter / digit ) *( letter / digit / "-" ) *( letter / digit )
//                 ; as specified in RFC 1123 [HNAME]
let pShortName : Parser<_> =
    (*
        abc
        abc-123
        abc-def-123

        abc-
        -abc
    *)
    pLetterOrDigit
    .>>. stringsSepBy (many1Chars pLetterOrDigit) (pstring "-")
    |>> (fun (start, rest) -> string start + rest)

// hostname   =  shortname *( "." shortname )
let pHostName : Parser<_> =
    stringsSepBy1 pShortName (pstring ".")

// servername =  hostname
let pServerName = pHostName

// could potentially allow for malformed IPV4 addresses, but this conforms to the IRC spec... hm...
// ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
let pIP4Addr : Parser<_> =
    let part : Parser<string> = manyMinMaxSatisfy 1 3 Char.IsDigit
    pipe4
        (part .>> pchar '.')
        (part .>> pchar '.')
        (part .>> pchar '.')
        part
        (fun a b c d -> sprintf "%s.%s.%s.%s" a b c d |> IPAddress.Parse)

// ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
// ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
let pIP6Addr : Parser<_> =
    let pTypical : Parser<_> =
        let part : Parser<string> = many1Chars pHexDigit
        part                 >>= fun a ->
        (pchar ':' >>. part) >>= fun b ->
        (pchar ':' >>. part) >>= fun c ->
        (pchar ':' >>. part) >>= fun d ->
        (pchar ':' >>. part) >>= fun e ->
        (pchar ':' >>. part) >>= fun f ->
        (pchar ':' >>. part) >>= fun g ->
        (pchar ':' >>. part) >>= fun h -> preturn (sprintf "%s:%s:%s:%s:%s:%s:%s:%s" a b c d e f g h |> IPAddress.Parse)
    let pIPv4Format : Parser<_> =
        pipe4
            (pstring "0:0:0:0:0:")
            (pstring "0" <|> pstring "FFFF")
            (pstring ":")
            pIP4Addr
            (fun a b c address -> IPAddress.Parse(a + b + c + string address))
            // TODO: ^^^ Absolutely nasty, I need a better way of doing this.
            // Maybe refactor IPV6/IPV4 to return string instead of address?
    pIPv4Format <|> pTypical

// hostaddr   =  ip4addr / ip6addr
let pHostAddr = (attempt pIP4Addr) <|> pIP6Addr

// host       =  hostname / hostaddr
let pHost = (attempt (pHostAddr |>> HostAddress)) <|> (pHostName |>> HostName)

// nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
let pNickName : Parser<string> =
    (pLetter <|> pSpecial)
    .>>. manyMinMaxSatisfy 0 8 (fun c ->
        List.contains c (letter @ digit @ special @ [ '-' ]))
    |>> (fun (start, rest) -> string start + rest)

// target     =  nickname / servername
let pTarget =
                                       // v prevents this parser from failing when it encounters a '.' which indicates that it is potentially a server name
    (pNickName .>>? notFollowedBy (pchar '.'))
    <|> pServerName

// user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
//                 ; any octet except NUL, CR, LF, " " and "@"
// in practice, it seems that [0-9] [a-z] [A-Z] [_] [-] or [.] are the only characters allowed
// for simplicty's sake, I'm just going to exclude '%' and '!', because it makes it easier to parse messages
let pUser : Parser<_> = many1Chars (noneOf [ '\x00'; '\r'; '\n'; ' '; '@'; '%'; '!' ])

// ( nickname [ [ "!" user ] "@" host ] )
let pPrefixUserPart : Parser<_> =
    pNickName .>>. opt (opt (pchar '!' >>. pUser) .>>. (pchar '@' >>. pHost))
    |>> (fun (nick, additional) ->
        let user, host =
            match additional with
            | Some (user, host) -> user, Some host
            | None -> None, None
        Prefix.User
            {
                NickName = nick
                User = user
                Host = host
            })
let pAllUserParts : Parser<_> =
    pipe3
        pNickName
        (pchar '!' >>. pUser)
        (pchar '@' >>. pHost)
        (fun nick user host ->
            {
                NickName = nick
                User = Some user
                Host = Some host
            }
        )

// middle     =  nospcrlfcl *( ":" / nospcrlfcl )
let pMiddle =
    pNoSpcCrLfCl .>>. manyChars (pchar ':' <|> pNoSpcCrLfCl)
    |>> (fun (a,b) -> string a + b)

// trailing   =  *( ":" / " " / nospcrlfcl )
let pTrailing = manyChars (anyOf [ ':'; ' ' ] <|> pNoSpcCrLfCl)

let pTrailingParam =
    pSpace >>. (fun stream ->
        if maxArgsReached stream.UserState then
            (optional (pchar ':') >>. pTrailing) stream
        else
            (pchar ':' >>. pTrailing) stream
    )

let pMiddleParamWithUserState : Parser<_> =
    pSpace
    .>>? notFollowedBy (pchar ':')
    .>> incrementArgCount
    .>>? userStateSatisfies maxArgsNotReached
    >>. pMiddle

// params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
// params     =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
let pParams : Parser<_> =
     many pMiddleParamWithUserState
     .>>. opt pTrailingParam
     .>> resetArgCount
     |>> (fun (middle, trailing) -> { Middle = middle; Trailing = trailing })

// prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
let pPrefix : Parser<_> =
    (pServerName .>>? notFollowedBy (anyOf (['!'; '@'] @ special))
    |>> (fun name ->
        // the only thing that differentiates the nickname case vs the server name case is the '.' character, which is not allowed in nicknames
        if name.Contains(".") then
            ServerName name
        else
            NickNameOrServerName name ))
    <|> pPrefixUserPart

// command    =  1*letter / 3digit
let pCommand : Parser<_> =
    (many1Chars pLetter |>> TextCommand)
    <|> (manyMinMaxSatisfy 3 3 Char.IsDigit |>> (int >> enum<ServerReply> >> IntCommand))

// message    =  [ ":" prefix SPACE ] command [ params ] crlf
let pMessage : Parser<_> =
    pipe3
        (opt (pchar ':' >>. pPrefix .>> pSpace))
        pCommand
        pParams
       (fun prefix command ``params`` -> { Prefix = prefix; Command = command; Params = ``params`` })
    .>> pCrLf

// channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
let pChannelId : Parser<_> =
    manyMinMaxSatisfy 5 5 (fun c -> List.contains c (upperLetter @ digit))

//chanstring = *49(%x01-06 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B / %x2D-39 / %x3B-FF)
// ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
let pChanString : Parser<_> =
    let valid =
        [ 0x00..0xFF ]
        |> List.map char
        |> List.except [ '\x00'; '\b'; '\r'; '\n'; ' '; ','; ':' ]
    manyMinMaxSatisfy 0 49 (fun c -> List.contains c valid)

let pChannelPrefix : Parser<_> =
    choice
        [
            pstring "#"
            pstring "+"
            pipe2 (pstring "!") pChannelId (+)
            pstring "&"
        ]

// channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring [ ":" chanstring ]
let pChannel : Parser<_> =
    pipe3
        pChannelPrefix
        pChanString
        (opt (pchar ':' >>. pChanString))
        (fun prefix name postfix -> { Prefix = prefix; Name = name; Postfix = postfix })

// matchone   =  %x01-FF
//                 ; matches wildone
let pMatchOne : Parser<_> = anyOf ['\x01'..'\xFF']

// matchmany  =  *matchone
//                 ; matches wildmany
let pMatchMany : Parser<_> = manyChars pMatchOne

// noesc      =  %x01-5B / %x5D-FF
//                 ; any octet except NUL and "\"
let pNoEsc : Parser<_> = noneOf noEsc

// nowild     =  %x01-29 / %x2B-3E / %x40-FF
//                 ; any octet except NUL, "*", "?"
let pNoWild : Parser<_> = noneOf [ '\x00'; '*'; '?' ]

// wildone    =  %x3F
let pWildOne : Parser<_> = pchar '?'

// wildmany   =  %x2A
let pWildMany : Parser<_> = pchar '*'

let previousCharNoEsc : Parser<_> = previousCharSatisfiesNot (fun c -> List.contains c noEsc)

// mask       =  *( nowild / noesc wildone / noesc wildmany )
let pMask =
    many <|
    choice
        [
            stringReturn "\\*" (NonWild '*')
            stringReturn "\\?" (NonWild '?')
            pNoWild |>> NonWild
            (pWildOne >>% WildOne)
            (pWildMany >>% WildMany)
        ]

let pTargetMaskTarget : Parser<_> =
    (charReturn '$' TargetType.Host) <|> (charReturn '#' TargetType.Channel)

// targetmask =  ( "$" / "#" ) mask
//                 ; see details on allowed masks in section 3.3.1
let pTargetMask : Parser<_> =
    pipe2
        pTargetMaskTarget
        pMask
        (fun target mask -> { Target = target; Mask = mask })

let pMsgToUserHostPart : Parser<_> =
    pchar '%' >>. pHost

let pMsgToServerNamePart : Parser<_> =
    pchar '@' >>. pServerName

let pMsgToUserInfo : Parser<Host option * ServerName option> = fun stream ->
    let reply = opt pMsgToUserHostPart stream
    if reply.Status <> Ok then
        Reply(reply.Status, reply.Error)
    else
        if stream.Peek() = '@' then
            let reply' = pMsgToServerNamePart stream
            if reply'.Status <> Ok then
                Reply(reply'.Status, reply'.Error)
            else
                Reply((reply.Result, Some reply'.Result))
        else if reply.Result = None then
            Reply(Error, messageError "Expected either a HostName, ServerName, or a HostName followed by a ServerName")
        else
            Reply((reply.Result, None))

let pMsgToUser : Parser<_> =
    pUser
    .>>. pMsgToUserInfo
    |>> (fun (user, info) ->
        let host, server = info
        {
            User = user
            Host = host
            ServerName = server
        }
    )

// msgto      =  channel / ( user [ "%" host ] "@" servername )
// msgto      =/ ( user "%" host ) / targetmask
// msgto      =/ nickname / ( nickname "!" user "@" host )
let pMsgTo : Parser<_> =
    choice
        [
            (attempt pTargetMask)   |>> TargetMask
            (attempt pChannel)      |>> Channel
            (attempt pMsgToUser)    |>> User
            (attempt pAllUserParts) |>> ServerUser
            (attempt pNickName)     |>> (fun nick -> ServerUser { NickName = nick; Host = None; User = None })
        ]

let pMsgTarget : Parser<_> = sepBy1 pMsgTo (pchar ',')

// key        =  1*23( %x01-08 / %x0E-1F / %x21-7F )
//                 ; any 7-bit US_ASCII character,
//                 ; except NUL, CR, LF, FF, h/v TABs, and " "
let pKey : Parser<_> =
    manyMinMaxSatisfy 1 23 (fun c ->
        List.contains c (
            ['\x00'..'\xFF']
            |> List.except [ '\x00'; '\r'; '\n'; '\f'; '\v'; '\t'; ' ' ]))

let parse parser str =
    match runParserOnString parser { ArgsParsed = 0 }  "" str with
    | Success(data, _, _) -> Result.Ok data
    | Failure(err, _, _) -> Result.Error err