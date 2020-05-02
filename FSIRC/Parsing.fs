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

let many1CharsStartsWith (start: Parser<char>) (rest: Parser<char>) : Parser<_> =
    start .>>. many1Chars rest
    |>> (fun (a,b) -> string a + b)

let manyCharsStartsWith (start: Parser<char>) (rest: Parser<char>) : Parser<_> =
    start .>>. manyChars rest
    |>> (fun (a,b) -> string a + b)

let upperLetter = ['A'..'Z']
let lowerLetter = ['a'..'z']

let letter = upperLetter @ lowerLetter

let digit = ['0'..'9']

let hexDigit = ['A'..'F'] @ ['a'..'f'] @ digit

let special = ['['..'`'] @ ['{'..'}']

let pLetter : Parser<_> = anyOf letter
let pDigit : Parser<_> = anyOf digit
let pLetterOrDigit : Parser<_> = anyOf (digit @ letter)
let pHexDigit = anyOf hexDigit
let pSpecial : Parser<_> = anyOf special

// nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
// ; any octet except NUL, CR, LF, " " and ":"
let pNoSpcCrLfCl : Parser<_> = noneOf [ '\x00'; '\r'; '\n'; ' '; ':'  ]

// SPACE      =  %x20        ; space character
let pSpace : Parser<_> = pchar ' '

// crlf       =  %x0D %x0A   ; "carriage return" "linefeed"
// https://bitbucket.org/fparsec/main/issues/18/cant-create-a-parser-to-parse-r-n-exactly
let pCrLf : Parser<_> =
    let error = expected "newline (\\r\\n)"
    let crcn = TwoChars('\r', '\n')
    fun stream ->
        if stream.Skip(crcn) then
            stream.RegisterNewline()
            Reply(())
        else
            Reply(Error, error)

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

let pHostName : Parser<_> =
    stringsSepBy1 pShortName (pstring ".")

let pServerName = pHostName

// could potentially allow for malformed IPV4 addresses, but this conforms to the IRC spec... hm...
let pIP4Addr : Parser<_> =
    let part : Parser<string> = manyMinMaxSatisfy 1 3 Char.IsDigit
    pipe4
        (part .>> pchar '.')
        (part .>> pchar '.')
        (part .>> pchar '.')
        part
        (fun a b c d -> sprintf "%s.%s.%s.%s" a b c d |> IPAddress.Parse)

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

let pHostAddr = (attempt pIP4Addr) <|> pIP6Addr

let pHost = (attempt (pHostAddr |>> HostAddress)) <|> (pHostName |>> HostName)

let pNickName : Parser<string> =
    (pLetter <|> pSpecial)
    .>>. manyMinMaxSatisfy 0 8 (fun c ->
        List.contains c (letter @ digit @ special @ [ '-' ]))
    |>> (fun (start, rest) -> string start + rest)

let pTarget =
                                               // v prevents this parser from failing when it encounters a '.' which indicates that it is potentially a server name
    (pNickName .>>? notFollowedBy (pchar '.'))
    <|> pServerName

// any octet except NUL, CR, LF, " " and "@"
// in practice, it seems that [0-9] [a-z] [A-Z] [_] [-] or [.] are the only characters allowed
// for simplicty's sake, I'm just going to exclude '%', because it makes it easier to parse messages
let pUser : Parser<_> = many1Chars (noneOf [ '\x00'; '\r'; '\n'; ' '; '@'; '%' ])

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

// middle     =  nospcrlfcl *( ":" / nospcrlfcl )
let pMiddle =
    manyCharsStartsWith pNoSpcCrLfCl (pchar ':' <|> pNoSpcCrLfCl)

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
    <|> (manyMinMaxSatisfy 3 3 Char.IsDigit |>> (uint32 >> IntCommand))

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

// targetmask =  ( "$" / "#" ) mask
//                 ; see details on allowed masks in section 3.3.1
let pTargetMask : Parser<_> = anyOf [ '$'; '#' ]