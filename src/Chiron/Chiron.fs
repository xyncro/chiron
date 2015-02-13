module Chiron

open Aether
open FParsec

(* Types

   Simple AST for JSON, with included isomorphisms in Aether format for
   lens/isomorphism based modification of complex JSON structures, plus the
   monadic signature of the "json" computation expression. *)

type Json =
    | JArray of Json list
    | JBool of bool
    | JNumber of float
    | JNull of unit
    | JObject of Map<string, Json>
    | JString of string

    static member JArrayPIso : PIso<Json, Json list> =
        (function | JArray x -> Some x
                  | _ -> None), JArray

    static member JBoolPIso : PIso<Json, bool> =
        (function | JBool x -> Some x
                  | _ -> None), JBool

    static member JNumberPIso : PIso<Json, float> =
        (function | JNumber x -> Some x
                  | _ -> None), JNumber

    static member JNullPIso : PIso<Json, unit> =
        (function | JNull () -> Some ()
                  | _ -> None), JNull

    static member JObjectPIso : PIso<Json, Map<string, Json>> =
        (function | JObject x -> Some x
                  | _ -> None), JObject

    static member JStringPIso : PIso<Json, string> =
        (function | JString x -> Some x
                  | _ -> None), JString

(* Parsing *)

[<AutoOpen>]
module Parsing =

    (* Conversion

       Functions for mapping escaped characters and strings to valid
       values. *)

    let private hexToInt x =
        (int x &&& 15) + (int x >>> 6) * 9

    let private hexString h3 h2 h1 h0 =
        (hexToInt h3) * 4096
        + (hexToInt h2) * 256
        + (hexToInt h1) * 16
        + (hexToInt h0)
        |> char
        |> string

    let private mapEsc =
        function | 'c' -> "\b"
                 | 'f' -> "\u000C"
                 | 'n' -> "\n"
                 | 'r' -> "\r"
                 | 't' -> "\t"
                 | c -> string c

    let private plainEscP = 
        anyOf "\"\\/bfnrt" |>> mapEsc

    let private unicodeEscP =
        pchar 'u' >>. pipe4 hex hex hex hex hexString

    let private escP =
        plainEscP <|> unicodeEscP

    let private stringP =
        manySatisfy (fun c -> c <> '"' && c <> '\\')

    let private quotedP =
        between (skipChar '"') (skipChar '"')

    let private literalP =
        quotedP (stringsSepBy stringP ((skipChar '\\') >>. escP))

    (* Patterns

       Functions for parsing items within patterns, such as JSON lists
       and JSON pairs. *)

    let private listItemP item =
        item .>> spaces

    let private listP o c item = 
        between o c (spaces >>. sepBy (listItemP item) (skipChar ',' .>> spaces))

    let private pairP item = 
        tuple2 literalP (spaces >>. pchar ':' >>. spaces >>. item)

    (* Parsers

       Functions for parsing JSON values to Json typed objects, given valid
       JSON data. *)

    let private jsonP, jsonRefP = 
        createParserForwardedToRef ()

    let private jArrayP =
        listP (skipChar '[') (skipChar ']') jsonP |>> JArray

    let private jBoolP = 
        (stringReturn "true" (JBool true)) <|> (stringReturn "false" (JBool false))

    let private jNullP = 
        stringReturn "null" (JNull ())

    let private jNumberP = 
        pfloat |>> JNumber 

    let private jObjectP = 
        listP (skipChar '{') (skipChar '}') (pairP jsonP) |>> (Map.ofList >> JObject)

    let private jStringP = 
        literalP |>> JString

    do jsonRefP := 
        choice [
            jObjectP
            jArrayP
            jStringP
            jBoolP
            jNumberP
            jNullP ]

    (* Functions

       Functions for parsing (or attempting to parse) JSON data as strings,
       returning data as Json types when successful (or as a wrapped Json type
       in the case of attempt-based parsing). *)

    [<RequireQualifiedAccess>]
    module Json =

        let tryParse json =
            match run jsonP json with
            | Success (json, _, _) -> Some json
            | Failure (e, _, _) -> None

        let parse =
               tryParse
            >> function | Some json -> json
                        | _ -> failwith "Failed Parse" 

(* Formatting *)

[<AutoOpen>]
module Formatting =

    let format json =
        ()

(* Monadic

   A computation expression based interface to Json data, providing
   lens based access to deeply nested elements of a Json data structure
   as part of the computation expression state (the monad is a state
   monad where the state is of type Json). *)

[<AutoOpen>]
module Monadic =

    type Json<'a> =
        Json -> Choice<'a, string> * Json

    (* Builder

       Computation expression (monad) for working with JSON structures in a
       simple way, including lensing, morphisms, etc. using the Aether
       library. *)

    type JsonBuilder () =

        member __.Bind (m1, m2) : Json<_> =
            fun json ->
                match m1 json with
                | Choice1Of2 x, json -> m2 x json
                | Choice2Of2 e, json -> Choice2Of2 e, json

        member __.Combine (m1, m2) : Json<_> =
            fun json ->
                match m1 json with
                | Choice1Of2 (), json -> m2 () json
                | Choice2Of2 e, json -> Choice2Of2 e, json

        member __.Delay (f) : Json<_> =
            fun json ->
                f () json

        member __.Return (x) : Json<_> =
            fun json -> 
                Choice1Of2 x, json

        member __.ReturnFrom (f) : Json<_> =
            f

        member __.Zero () : Json<_> =
            fun json ->
                Choice1Of2 (), json

    let json =
        JsonBuilder ()

    (* Functions

       Computation expression (monadic) functions for working with the Json
       structure maintained as monadic state. *)

    [<RequireQualifiedAccess>]
    module Json =

        let internal succeed x : Json<_> =
            fun json ->
                Choice1Of2 x, json

        let internal fail e : Json<_> =
            fun json ->
                Choice2Of2 e, json

        let get l : Json<_> =
            fun json ->
                Choice1Of2 (Lens.get l json), json

        let getPartial l : Json<_> =
            fun json ->
                Choice1Of2 (Lens.getPartial l json), json

        let set l v : Json<_> =
            fun json ->
                Choice1Of2 (), Lens.set l v json

        let setPartial l v : Json<_> =
            fun json ->
                Choice1Of2 (), Lens.setPartial l v json

        let map l f : Json<_> =
            fun json ->
                Choice1Of2 (), Lens.map l f json

        let mapPartial l f : Json<_> =
            fun json ->
                Choice1Of2 (), Lens.mapPartial l f json