module Tests

open Expecto
open FSIRC
open FParsec
open FSIRC.Parsing
open System.Net

[<Tests>]
let ``basic parsing`` =
    testList "basic parsing" [
        testList "primitives" [
            testCase "pShortName compared"
            <| Helpers.parseAndCompare (pShortName .>> eof)
                [
                    ("1", "1")
                    ("123", "123")
                    ("12-3", "12-3")
                    ("abc-def-123-456", "abc-def-123-456")
                ]
            testCase "pShortName failures"
            <| Helpers.parseAndExpectFailure (pShortName .>> eof)
                [
                    "-abc"
                    "abc-"
                    "abc-def-123-"
                    "@bc-@ef-123"
                    "a--b--c"
                ]
            testCase "pHostName compared"
            <| Helpers.parseAndCompare (pHostName .>> eof)
                [
                    "abc.123.def.456", "abc.123.def.456"
                    "abc-123.def-456", "abc-123.def-456"
                    "abc-456", "abc-456"
                    "abc-123.def.456", "abc-123.def.456"
                    "irc.foonet.com", "irc.foonet.com"
                    "csd.bu.edu", "csd.bu.edu"
                    "tolsun.oulu.fi", "tolsun.oulu.fi"
                    "ircd.stealth.net", "ircd.stealth.net"
                ]
            testCase "pHostName failures"
            <| Helpers.parseAndExpectFailure (pHostName .>> eof)
                [
                    ".abc"
                    "abc."
                    "ab..c"
                    "a.b.c.d.."
                ]
            testCase "pIPv4 compared"
            <| Helpers.parseAndCompare (pIP4Addr .>> eof)
                ([
                    "192.168.0.1"
                    "0.0.0.0"
                    "0.255.255.255"
                    "10.0.0.0"
                    "10.255.255.255"
                    "100.64.0.0"
                    "100.127.255.255"
                    "127.0.0.0"
                    "127.255.255.255"
                    "169.254.0.0"
                    "169.254.255.255"
                    "172.16.0.0"
                    "172.31.255.255"
                    "192.0.0.0"
                    "192.0.0.255"
                    "192.0.2.0"
                    "192.0.2.255"
                    "192.88.99.0"
                    "192.88.99.255"
                    "192.168.0.0"
                    "192.168.255.255"
                    "198.18.0.0"
                    "198.19.255.255"
                    "198.51.100.0"
                    "198.51.100.255"
                    "203.0.113.0"
                    "203.0.113.255"
                    "224.0.0.0"
                    "239.255.255.255"
                    "240.0.0.0"
                    "255.255.255.254"
                    "255.255.255.255"
                ]
                |> List.map (fun str -> (str, IPAddress.Parse(str))))
            testCase "pIPv6 compared"
            <| Helpers.parseAndCompare (pIP6Addr .>> eof)
                ([
                    "fd5d:b9f8:76e1:fc40:0000:0000:0000:0000"
                    "fd5d:b9f8:76e1:fc40:ffff:ffff:ffff:ffff"
                    "0:0:0:0:0:0:192.168.0.1"
                    "0:0:0:0:0:FFFF:192.168.0.1"
                ]
                |> List.map (fun str -> (str, IPAddress.Parse(str))))
            testCase "pHostAddress compared"
            <| Helpers.parseAndCompare (pHostAddr .>> eof)
                ([
                    "fd5d:b9f8:76e1:fc40:0000:0000:0000:0000"
                    "fd5d:b9f8:76e1:fc40:ffff:ffff:ffff:ffff"
                    "0:0:0:0:0:0:192.168.0.1"
                    "0:0:0:0:0:FFFF:192.168.0.1"
                    "239.255.255.255"
                    "240.0.0.0"
                    "255.255.255.254"
                    "255.255.255.255"
                ]
                |> List.map (fun str -> (str, IPAddress.Parse(str))))
            testCase "pHost compared"
            <| Helpers.parseAndCompare (pHost .>> eof)
                (
                    ([
                        "239.255.255.255"
                        "240.0.0.0"
                        "255.255.255.254"
                        "255.255.255.255"
                        "fd5d:b9f8:76e1:fc40:0000:0000:0000:0000"
                        "fd5d:b9f8:76e1:fc40:ffff:ffff:ffff:ffff"
                        "0:0:0:0:0:0:192.168.0.1"
                        "0:0:0:0:0:FFFF:192.168.0.1"
                    ]
                    |> List.map (fun str -> str, str |> IPAddress.Parse |> HostAddress))
                    @
                    ([
                        "abc"
                        "abc.123.def.456"
                        "abc-123.def-456"
                        "abc-456"
                        "abc-123.def.456"
                        "irc.foonet.com"
                        "csd.bu.edu"
                        "tolsun.oulu.fi"
                        "ircd.stealth.net"
                    ]
                    |> List.map (fun str -> str, HostName str)))
            testCase "pNickName compared"
            <| Helpers.parseAndCompare (pNickName .>> eof)
                [
                    "a23456789", "a23456789"
                    "a", "a"
                    "bcde{}", "bcde{}"
                    "{Super-}", "{Super-}"
                    "john-ab", "john-ab"
                    "[cool]", "[cool]"
                    "`Ne4t^1", "`Ne4t^1"
                    "[]\\`_^{|", "[]\\`_^{|"
                    "}cool{", "}cool{"
                ]
            testCase "pNickName failures"
            <| Helpers.parseAndExpectFailure (pNickName .>> eof)
                [
                    "0123"
                    "-COOL"
                    "\x02abc"
                ]
            testCase "pTarget compared"
            <| Helpers.parseAndCompare (pTarget .>> eof)
                [
                    "a23456789", "a23456789"
                    "a", "a"
                    "bcde{}", "bcde{}"
                    "{Super-}", "{Super-}"
                    "john-ab", "john-ab"
                    "[cool]", "[cool]"
                    "`Ne4t^1", "`Ne4t^1"
                    "[]\\`_^{|", "[]\\`_^{|"
                    "}cool{", "}cool{"
                    
                    "abc.123.def.456", "abc.123.def.456"
                    "abc-123.def-456", "abc-123.def-456"
                    "abc-456", "abc-456"
                    "abc-123.def.456", "abc-123.def.456"
                    "irc.foonet.com", "irc.foonet.com"
                    "csd.bu.edu", "csd.bu.edu"
                    "tolsun.oulu.fi", "tolsun.oulu.fi"
                    "ircd.stealth.net", "ircd.stealth.net"
                ]
            testCase "pUser compared"
            <| Helpers.parseAndCompare (pUser .>> eof)
                [
                    "a23456789", "a23456789"
                    "a", "a"
                    "bcde{}aaaaaaaaaaaa", "bcde{}aaaaaaaaaaaa"
                    "~!#$%^&*()_+{}|:\"<>?QWERetbezxstvxzc", "~!#$%^&*()_+{}|:\"<>?QWERetbezxstvxzc"
                ]
            testCase "pUser failures"
            <| Helpers.parseAndExpectFailure (pUser .>> eof)
                [
                    ""
                    "\x00COOL"
                    "\rNEAT"
                    "abc\n"
                    "Dont@Me"
                    "Kevin y"
                ]
            testCase "pCommand compared"
            <| Helpers.parseAndCompare (pCommand .>> eof)
                [
                    "A", TextCommand "A"
                    "BCDE", TextCommand "BCDE"
                    "FGHIJKLMNOPQRSTUVWXyzrfestghtjyafeadrwvkjzokx", TextCommand "FGHIJKLMNOPQRSTUVWXyzrfestghtjyafeadrwvkjzokx"
                    "001", IntCommand 1u
                    "500", IntCommand 500u
                    "999", IntCommand 999u
                ]
            testCase "pMiddle compared"
            <| Helpers.parseAndCompare (pMiddle .>> eof)
                [
                    "Abwaerwet5634673875r89ftungh23523@#$!!#^#*5", "Abwaerwet5634673875r89ftungh23523@#$!!#^#*5"
                    (['\x00'..'\xFF'] |> List.except [ '\x00'; '\r'; '\n'; ' '; ':' ] |> List.map string |> String.concat ""), (['\x00'..'\xFF'] |> List.except [ '\x00'; '\r'; '\n'; ' '; ':' ] |> List.map string |> String.concat "")
                    "A:Really:Cool:Colon:Chain:", "A:Really:Cool:Colon:Chain:"
                ]
            testCase "pMiddle failures"
            <| Helpers.parseAndExpectFailure (pMiddle .>> eof)
                [
                    ":OhNoIStartedWithAColon!"
                    "\x00NotCool"
                    "I contain spaces!"
                    "Newlines?That'sAPaddlin'\r\n"
                    "Newlines?That'sAPaddlin'\n"
                    "Newlines?That'sAPaddlin'\r"
                ]
            testCase "pTrailing compared"
            <| Helpers.parseAndCompare (pTrailing .>> eof)
                [
                    "WOW! I GET TO PARSE THINGS WITH SPACES AND COLONS NOW!", "WOW! I GET TO PARSE THINGS WITH SPACES AND COLONS NOW!"
                    ":WOW! I GET TO PARSE THINGS WITH SPACES AND COLONS NOW!", ":WOW! I GET TO PARSE THINGS WITH SPACES AND COLONS NOW!"
                ]
            testCase "pTrailing failures"
            <| Helpers.parseAndExpectFailure (pTrailing .>> eof)
                [
                    "I still don't get to have newlines, though \r\n"
                    "I still don't get to have newlines, though \n"
                    "I still don't get to have newlines, though \r"
                    "Or naughty nulls!\x00"
                ]
            testCase "pTrailingParam" (fun _ ->
                [
                    " :Trailing !", "Trailing !", { ArgsParsed = 1 }
                    " :Trailing !", "Trailing !", { ArgsParsed = 14 }
                    " trailing", "trailing", { ArgsParsed = 14 }
                ]
                |> List.iter (fun (input, expected, state) ->
                    let result =
                        Helpers.runWithState state (pTrailingParam .>> eof) input
                        |> Helpers.unwrap
                    Expect.equal result expected "Not equal!"
                )
            )
        ]

        testList "prefix parsing" [
            testCase "servername"
            <| Helpers.parseAndCompare (pPrefix .>> eof)
                [
                    "irc.foonet.com", Prefix.ServerName "irc.foonet.com"
                    "csd.bu.edu", Prefix.ServerName "csd.bu.edu"
                    "tolsun.oulu.fi", Prefix.ServerName "tolsun.oulu.fi"
                    "ircd.stealth.net", Prefix.ServerName "ircd.stealth.net"
                    
                    "abc.123.def.456", Prefix.ServerName "abc.123.def.456"
                    "abc-123.def-456", Prefix.ServerName "abc-123.def-456"
                    "abc-123.def.456", Prefix.ServerName "abc-123.def.456"
                ]
            testCase "nickname only (ambiguous cases!)"
            <| Helpers.parseAndCompare (pPrefix .>> eof)
                [
                    "WiZ", NickNameOrServerName "WiZ"
                    "syrk", NickNameOrServerName "syrk"
                    "Angel", NickNameOrServerName "Angel"
                    "abc-456", NickNameOrServerName "abc-456"
                ]
            testCase "nickname with host"
            <| Helpers.parseAndCompare (pPrefix .>> eof)
                [
                    ("WiZ@tolsun.oulu.fi", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostName "tolsun.oulu.fi") }))
                    ("syrk@millennium.stealth.net", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostName "millennium.stealth.net") }))
                    ("Angel@irc.org", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostName "irc.org") }))
        
                    ("WiZ@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
                    ("syrk@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
                    ("Angel@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        
                    ("WiZ@192.168.0.1", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                    ("syrk@192.168.0.1", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                    ("Angel@192.168.0.1", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                ]
            testCase "nickname with user and host"
            <| Helpers.parseAndCompare (pPrefix .>> eof)
                [
                    ("WiZ!jto@tolsun.oulu.fi", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostName "tolsun.oulu.fi") }))
                    ("syrk!kalt@millennium.stealth.net", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostName "millennium.stealth.net") }))
                    ("Angel!wings@irc.org", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostName "irc.org") }))
        
                    ("WiZ!jto@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
                    ("syrk!kalt@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
                    ("Angel!wings@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        
                    ("WiZ!jto@192.168.0.1", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                    ("syrk!kalt@192.168.0.1", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                    ("Angel!wings@192.168.0.1", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
                ]
        ]

    ]
