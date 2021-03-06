# Parser Checklist:
| Rule       | Parser implemented?  |
| ---------- | -------------------- |
| message    |          ✔           |
| prefix     |          ✔           |
| command    |          ✔           |
| params     |          ✔           |
| nospcrlfcl |          ✔           |
| middle     |          ✔           |
| trailing   |          ✔           |
| SPACE      |          ✔           |
| crlf       |          ✔           |
| target     |          ✔           |
| msgtarget  |          ✔           |
| msgto      |          ✔           |
| channel    |          ✔           |
| servername |          ✔           |
| host       |          ✔           |
| hostname   |          ✔           |
| shortname  |          ✔           |
| hostaddr   |          ✔           |
| ip4addr    |          ✔           |
| ip6addr    |          ✔           |
| nickname   |          ✔           |
| targetmask |          ✔           |
| chanstring |          ✔           |
| channelid  |          ✔           |
| user       |          ✔           |
| key        |          ✔           |
| letter     |          ✔           |
| digit      |          ✔           |
| hexdigit   |          ✔           |
| special    |          ✔           |
| mask       |          ✔           |
| wildone    |          ✔           |
| wildmany   |          ✔           |
| nowild     |          ✔           |
| noesc      |          ✔           |
| matchone   |          ✔           |
| matchmany  |          ✔           |

# IRC ABNF Spec
```
; Pulled from https://tools.ietf.org/html/rfc2812#section-3
; With some Errata applied:
; * https://www.rfc-editor.org/errata/eid4289
; * https://www.rfc-editor.org/errata/eid3783
; * https://www.rfc-editor.org/errata/eid4836

;   The extracted message is parsed into the components <prefix>,
;   <command> and list of parameters (<params>).
;
;    The Augmented BNF representation for this is:

message    =  [ ":" prefix SPACE ] command [ params ] crlf
prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
command    =  1*letter / 3digit
params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
params     =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                ; any octet except NUL, CR, LF, " " and ":"
middle     =  nospcrlfcl *( ":" / nospcrlfcl )
trailing   =  *( ":" / " " / nospcrlfcl )

SPACE      =  %x20        ; space character
crlf       =  %x0D %x0A   ; "carriage return" "linefeed"


; Most protocol messages specify additional semantics and syntax for
;   the extracted parameter strings dictated by their position in the
;   list.  For example, many server commands will assume that the first
;   parameter after the command is the list of targets, which can be
;   described with:
target     =  nickname / servername
msgtarget  =  msgto *( "," msgto )
msgto      =  channel / ( user [ "%" host ] "@" servername )
msgto      =/ ( user "%" host ) / targetmask
msgto      =/ nickname / ( nickname "!" user "@" host )
channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring [ ":" chanstring ]
servername =  hostname
host       =  hostname / hostaddr
hostname   =  shortname *( "." shortname )
shortname  =  ( letter / digit ) *( letter / digit / "-" ) *( letter / digit )
                ; as specified in RFC 1123 [HNAME]
hostaddr   =  ip4addr / ip6addr
ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
targetmask =  ( "$" / "#" ) mask
                ; see details on allowed masks in section 3.3.1
chanstring = *49(%x01-06 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B / %x2D-39 / %x3B-FF)
                ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

; Other parameter syntaxes are:
user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                ; any octet except NUL, CR, LF, " " and "@"
key        =  1*23( %x01-08 / %x0E-1F / %x21-7F )
                ; any 7-bit US_ASCII character,
                ; except NUL, CR, LF, FF, h/v TABs, and " "
letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
digit      =  %x30-39                 ; 0-9
hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
special    =  %x5B-60 / %x7B-7D
                ; "[", "]", "\", "`", "_", "^", "{", "|", "}"


; 2.5 Wildcard expressions
;
;    When wildcards are allowed in a string, it is referred as a "mask".
;
;    For string matching purposes, the protocol allows the use of two
;    special characters: '?' (%x3F) to match one and only one character,
;    and '*' (%x2A) to match any number of any characters.  These two
;    characters can be escaped using the character '\' (%x5C).
;
;    The Augmented BNF syntax for this is:
mask       =  *( nowild / noesc wildone / noesc wildmany )
wildone    =  %x3F
wildmany   =  %x2A
nowild     =  %x01-29 / %x2B-3E / %x40-FF
                ; any octet except NUL, "*", "?"
noesc      =  %x01-5B / %x5D-FF
                ; any octet except NUL and "\"
matchone   =  %x01-FF
                ; matches wildone
matchmany  =  *matchone
                ; matches wildmany
```
