namespace Chiron

open Aether
open Aether.Operators


[<AutoOpen>]
module AST =

    type JSON =
        | JArray of JSON list
        | JBool of bool
        | JNumber of float
        | JNull
        | JObject of Map<string, JSON>
        | JString of string

    // Patterns

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

    // Lenses

    let isojArrayPLens =
        isoPLens (|JArrayP|) JArray

    let isoJBoolPLens =
        isoPLens (|JBoolP|) JBool

    let isoJNullPLens =
        isoPLens (|JNullP|) (fun _ -> JNull)

    let isoJNumberPLens =
        isoPLens (|JNumberP|) JNumber

    let isoJObjectPLens =
        isoPLens (|JObjectP|) JObject

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

    let inline getM =
        fun j ->
            Choice1Of2 j, j

    let inline setM j =
        fun _ ->
            Choice1Of2 (), j

    let inline modM f =
        fun j ->
            Choice1Of2 (), f j

    let getPLM l =
        fun j -> 
            (match getPL l j with
            | Some x -> Choice1Of2 x 
            | _ -> Choice2Of2 "Value Not Found"), j

    let setPLM l v = 
        fun j -> Choice1Of2 (), setPL l v j

    let modPLM l f = 
        fun j -> Choice1Of2 (), modPL l f j
