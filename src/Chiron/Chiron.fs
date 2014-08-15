namespace Chiron


[<AutoOpen>]
module Core =

    open Aether
    open Aether.Operators


    type JSON =
        | JArray of JSON list
        | JBool of bool
        | JNumber of float
        | JNull
        | JObject of Map<string, JSON>
        | JString of string

    
    [<AutoOpen>]
    module Patterns =

        let (|JArrayP|) = 
            function 
            | JArray a -> Some a 
            | _ -> None

        let (|JBoolP|) = 
            function 
            | JBool b -> Some b 
            | _ -> None

        let (|JNumberP|) = 
            function 
            | JNumber n -> Some n 
            | _ -> None

        let (|JNullP|) = 
            function 
            | JNull -> Some () 
            | _ -> None

        let (|JObjectP|) = 
            function 
            | JObject o -> Some o 
            | _ -> None

        let (|JStringP|) = 
            function 
            | JString s -> Some s 
            | _ -> None


    [<AutoOpen>]
    module Lenses =

        let jArrayPLens i =
            isoPLens (|JArrayP|) JArray >??> listPLens i

        let jObjectPLens k =
            isoPLens (|JObjectP|) JObject >??> mapPLens k

        let isoJBoolPLens =
            isoPLens (|JBoolP|) JBool

        let isoJNumberPLens =
            isoPLens (|JNumberP|) JNumber

        let isoJStringPLens =
            isoPLens (|JStringP|) JString


    [<AutoOpen>]
    module Monad =
    
        type JSONFunc<'T> =
            JSON -> Choice<'T, string> * JSON

        type JSONBuilder () =

            member x.Return v : JSONFunc<'T> =
                fun json -> Choice1Of2 v, json

            member x.ReturnFrom f : JSONFunc<_> =
                f

            member x.Bind (f: JSONFunc<_>, k: 'T -> JSONFunc<_>) : JSONFunc<'U> =
                fun json ->
                    match f json with
                    | Choice1Of2 value, json -> k value json
                    | Choice2Of2 error, json -> Choice2Of2 error, json
        
            member x.Combine (r1: JSONFunc<_>, r2: JSONFunc<_>) : JSONFunc<'T> =
                x.Bind (r1, (fun () -> r2))

        
            member x.Delay (f: unit -> JSONFunc<_>) : JSONFunc<'T> =
                fun json -> f () json

            member x.Zero () : JSONFunc<unit> =
                fun json -> Choice1Of2 (), json
                            
        let json = 
            JSONBuilder ()

        let choice1 x = 
            fun j -> Choice1Of2 x, j

        let choice2 x = 
            fun j -> Choice2Of2 x, j

        let getM l =
            fun j -> 
                (match getPL l j with
                | Some x -> Choice1Of2 x 
                | _ -> Choice2Of2 "Value Not Found"), j

        let setM l v = 
            fun j -> Choice1Of2 (), setPL l v j

        let modM l f = 
            fun j -> Choice1Of2 (), modPL l f j
        

[<AutoOpen>]
module Serialization =

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


[<AutoOpen>]
module Mapping =

    open System
    open Aether
    open Aether.Operators

    
    [<AutoOpen>]
    module Defaults =           

        type Defaults = Defaults with
        
            // bool

            static member fromJSON (_: bool) = 
                getM isoJBoolPLens

            static member toJSON (x: bool) = 
                setM isoJBoolPLens x

            // decimal

            static member fromJSON (_: decimal) = 
                getM (isoJNumberPLens >?-> isoLens decimal float)

            static member toJSON (x: decimal) = 
                setM (isoJNumberPLens >?-> isoLens decimal float) x

            // float

            static member fromJSON (_: float) =
                getM isoJNumberPLens

            static member toJSON (x: float) =
                setM isoJNumberPLens x

            // int

            static member fromJSON (_: int) = 
                getM (isoJNumberPLens >?-> isoLens int float)

            static member toJSON (x: int) = 
                setM (isoJNumberPLens >?-> isoLens int float) x

            // int16

            static member fromJSON (_: int16) = 
                getM (isoJNumberPLens >?-> isoLens int16 float)

            static member toJSON (x: int16) = 
                setM (isoJNumberPLens >?-> isoLens int16 float) x

            // int64

            static member fromJSON (_: int64) = 
                getM (isoJNumberPLens >?-> isoLens int64 float)

            static member toJSON (x: int64) = 
                setM (isoJNumberPLens >?-> isoLens int64 float) x

            // string

            static member fromJSON (_: string) =
                getM isoJStringPLens

            static member toJSON (x: string) =
                setM isoJStringPLens x
            
            // uint16

            static member fromJSON (_: uint16) = 
                getM (isoJNumberPLens >?-> isoLens uint16 float)

            static member toJSON (x: uint16) = 
                setM (isoJNumberPLens >?-> isoLens uint16 float) x

            // uint32

            static member fromJSON (_: uint32) = 
                getM (isoJNumberPLens >?-> isoLens uint32 float)

            static member toJSON (x: uint32) = 
                setM (isoJNumberPLens >?-> isoLens uint32 float) x

            // uint64

            static member fromJSON (_: uint64) = 
                getM (isoJNumberPLens >?-> isoLens uint64 float)

            static member toJSON (x: uint64) = 
                setM (isoJNumberPLens >?-> isoLens uint64 float) x


    [<AutoOpen>]
    module internal Functions =

        let inline fromJSONWithDefaults (a: ^a, b: ^b) =
            ((^a or ^b) : (static member fromJSON: ^b -> ^b JSONFunc) b)

        let inline toJSONWithDefaults (a: ^a, b: ^b) =
            ((^a or ^b) : (static member toJSON: ^b -> unit JSONFunc) b)


    let inline fromJSON (x: JSON) : Choice<'a, string> = 
        fromJSONWithDefaults (Defaults, Unchecked.defaultof<'a>) x |> fst

    let inline toJSON (x: 'a) : JSON = 
        toJSONWithDefaults (Defaults, x) (JObject (Map.empty)) |> snd


    [<AutoOpen>]
    module Operators =

        let inline (!!) k =
            json {
                let! x = getM (jObjectPLens k)

                match fromJSON x with
                | Choice1Of2 x -> return! choice1 x
                | Choice2Of2 x -> return! choice2 x }

        let inline (<!) k v =
            json {
                do! setM (jObjectPLens k) (toJSON v) }
