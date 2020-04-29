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
            testCase "pShortName failures" (fun _ ->
                [
                    "-abc"
                    "abc-"
                    "abc-def-123-"
                    "@bc-@ef-123"
                    "a--b--c"
                ]
                |> List.iter (fun testStr ->
                    let result = Helpers.run (pShortName .>> eof) testStr
                    Expect.isFailure result "Should not parse successfully"
                )
            )
        ]
        // testList "prefix parsing" [
        //     testCase "servername"
        //     <| Helpers.parseAndCompare pPrefix
        //         [
        //             ("irc.foonet.com", Prefix.ServerName "irc.foonet.com")
        //             ("csd.bu.edu", Prefix.ServerName "csd.bu.edu")
        //             ("tolsun.oulu.fi", Prefix.ServerName "tolsun.oulu.fi")
        //             ("ircd.stealth.net", Prefix.ServerName "ircd.stealth.net")
        //         ]
        //     testCase "nickname"
        //     <| Helpers.parseAndCompare pPrefix
        //         [
        //             ("WiZ", Prefix.User ({ NickName = "WiZ"; User = None; Host = None }))
        //             ("syrk", Prefix.User ({ NickName = "syrk"; User = None; Host = None }))
        //             ("Angel", Prefix.User ({ NickName = "Angel"; User = None; Host = None }))
        //         ]
        //     testCase "nickname with host"
        //     <| Helpers.parseAndCompare pPrefix
        //         [
        //             ("WiZ@tolsun.oulu.fi", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostName "tolsun.oulu.fi") }))
        //             ("syrk@millennium.stealth.net", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostName "millennium.stealth.net") }))
        //             ("Angel@irc.org", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostName "irc.org") }))
//
        //             ("WiZ@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        //             ("syrk@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        //             ("Angel@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
//
        //             ("WiZ@192.168.0.1", Prefix.User ({ NickName = "WiZ"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //             ("syrk@192.168.0.1", Prefix.User ({ NickName = "syrk"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //             ("Angel@192.168.0.1", Prefix.User ({ NickName = "Angel"; User = None; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //         ]
        //     testCase "nickname with user and host"
        //     <| Helpers.parseAndCompare pPrefix
        //         [
        //             ("WiZ!jto@tolsun.oulu.fi", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostName "tolsun.oulu.fi") }))
        //             ("syrk!kalt@millennium.stealth.net", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostName "millennium.stealth.net") }))
        //             ("Angel!wings@irc.org", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostName "irc.org") }))
//
        //             ("WiZ!jto@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        //             ("syrk!kalt@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
        //             ("Angel!wings@2600:1005:b062:61e4:74d7:f292:802c:fbfd", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostAddress (IPAddress.Parse("2600:1005:b062:61e4:74d7:f292:802c:fbfd")))}))
//
        //             ("WiZ!jto@192.168.0.1", Prefix.User ({ NickName = "WiZ"; User = Some "jto"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //             ("syrk!kalt@192.168.0.1", Prefix.User ({ NickName = "syrk"; User = Some "kalt"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //             ("Angel!wings@192.168.0.1", Prefix.User ({ NickName = "Angel"; User = Some "wings"; Host = Some (HostAddress (IPAddress.Parse("192.168.0.1")) )}))
        //         ]
        // ]

    ]
