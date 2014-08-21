namespace Chiron

open System.Text
open FParsec


[<AutoOpen>]
module internal Symbols =

    let colon, comma, quote, slash = ":", ",", "\"", "\\" 
    let braceOpen, braceClose = "{", "}"
    let bracketOpen, bracketClose = "[", "]"

    let pColon, pComma, pQuote, pSlash = pstring colon, pstring comma, pstring quote, pstring slash
    let pBraceOpen, pBraceClose = pstring braceOpen, pstring braceClose
    let pBracketOpen, pBracketClose = pstring bracketOpen, pstring bracketClose


[<AutoOpen>]
module Reader =


    [<AutoOpen>]
    module internal Strings =

        let hexToInt x =
            (int x &&& 15) + (int x >>> 6) * 9

        let hexString h3 h2 h1 h0 =
            (hexToInt h3) * 4096
            + (hexToInt h2) * 256
            + (hexToInt h1) * 16
            + (hexToInt h0)
            |> char
            |> string

        let mapEscape c =
            match c with
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c   -> string c

        let escape = 
            anyOf "\"\\/bfnrt" |>> mapEscape

        let unicodeEscape =
            pchar 'u' >>. pipe4 hex hex hex hex hexString

        let isStringChar c =
            c <> '"' && c <> '\\' 

        let quoted =
            between pQuote pQuote

        let literal =
            quoted (stringsSepBy (manySatisfy isStringChar)
                                    (pSlash >>. (escape <|> unicodeEscape)))


    [<AutoOpen>]
    module internal Patterns =

        let list o c item = 
            between o c (spaces >>. sepBy (item .>> spaces) (pComma .>> spaces))

        let pair item = 
            tuple2 literal (spaces >>. pColon >>. spaces >>. item)


    [<AutoOpen>]
    module internal Parsers =

        let pValue, pValueRef = 
            createParserForwardedToRef ()

        let pArray = 
            list pBracketOpen pBracketClose pValue |>> JArray

        let pBool = 
            (stringReturn "true" (JBool true)) <|> (stringReturn "false" (JBool false))

        let pNull = 
            stringReturn "null" JNull

        let pNumber = 
            pfloat |>> JNumber 

        let pObject = 
            list pBraceOpen pBraceClose (pair pValue) |>> (Map.ofList >> JObject)

        let pString = 
            literal |>> JString

        let pJSON = 
            spaces >>. pValue .>> spaces .>> eof

        do pValueRef := 
            choice [ pObject; pArray; pString; pNumber; pBool; pNull ]


    let readJSON str = 
        match run pJSON str with
        | Success (x, _, _) -> Choice1Of2 x
        | Failure (err, _, _) -> Choice2Of2 err


[<AutoOpen>]
module Writer =

    let rec private writeArray arr =
        fun (x: StringBuilder) ->
            let l = List.length arr - 1
            let x = x.Append "["

            List.iteri (fun i json ->
                match i = l with
                | true -> (write json) x |> ignore
                | _ -> (write json >> writeLiteral ",") x |> ignore) arr

            let x = x.Append "]"
            x

    and private writeBool b =
        fun (x: StringBuilder) ->
            match b with
            | true -> x.Append "true"
            | _ -> x.Append "false"

    and private writeLiteral (l: string) =
        fun (x: StringBuilder) ->
            x.Append l

    and private writeNumber f =
        fun (x: StringBuilder) ->
            x.Append (string f)

    and private writeNull =
        fun (x: StringBuilder) ->
            x.Append "null"

    and private writeObject o =
        fun (x: StringBuilder) ->
            let arr = Map.toList o
            let l = List.length arr - 1
            let x = x.Append "{"

            List.iteri (fun i (k, v) ->
                match i = l with
                | true -> (writeString k >> writeLiteral ":" >> write v) x |> ignore
                | _ -> (writeString k >> writeLiteral ":" >> write v >> writeLiteral ",") x |> ignore) arr

            let x = x.Append "}"
            x

    and private writeString s =
        fun (x: StringBuilder) ->
            x.Append (sprintf "\"%s\"" s)

    and private write json =
        match json with
        | JArray x -> writeArray x
        | JBool x -> writeBool x
        | JNumber x -> writeNumber x
        | JNull -> writeNull
        | JObject x -> writeObject x
        | JString x -> writeString x

    let writeJSON (json: JSON) =
        write json (StringBuilder ()) |> string
