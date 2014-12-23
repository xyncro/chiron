module Chiron

(* Types

   Simple AST for JSON, with included isomorphisms in Aether format for 
   lens/isomorphism based modification of complex JSON structures, plus the
   monadic signature of the "json" computation expression. *)

type JSON =
    | JArray of JSON list
    | JBool of bool
    | JNumber of float
    | JNull of unit
    | JObject of Map<string, JSON>
    | JString of string

    static member JArrayPIso =
        (function | JArray x -> Some x
                  | _ -> None), JArray

    static member JBoolPIso =
        (function | JBool x -> Some x
                  | _ -> None), JBool

    static member JNumberPIso =
        (function | JNumber x -> Some x
                  | _ -> None), JNumber

    static member JNullPIso =
        (function | JNull () -> Some ()
                  | _ -> None), JNull

    static member JObjectPIso =
        (function | JObject x -> Some x
                  | _ -> None), JObject

    static member JStringPIso =
        (function | JString x -> Some x
                  | _ -> None), JString

type JSON<'a> =
    JSON -> Choice<'a, string> * JSON

(* Computation Expression

   Computation expression (monad) for working with JSON structures in a simple way, 
   including lensing, morphisms, etc. using the Aether library. *)

type JSONBuilder () =

    member __.Bind (m1, m2) : JSON<_> =
        fun json ->
            match m1 json with
            | Choice1Of2 x, json -> m2 x json
            | Choice2Of2 e, json -> Choice2Of2 e, json

    member __.Combine (m1, m2) : JSON<_> =
        fun json ->
            match m1 json with
            | Choice1Of2 (), json -> m2 () json
            | Choice2Of2 e, json -> Choice2Of2 e, json

    member __.Delay (f) : JSON<_> =
        fun json ->
            f () json

    member __.Return (x) : JSON<_> =
        fun json -> 
            Choice1Of2 x, json

    member __.ReturnFrom (f) : JSON<_> =
        f

    member __.Zero () : JSON<_> =
        fun json ->
            Choice1Of2 (), json

let json = 
    JSONBuilder ()

(* Computation Expression Functions

   Computation expression (monadic) functions for working with the JSON
   structure maintained as monadic state. *)

let succeed x : JSON<_> =
    fun json ->
        Choice1Of2 x, json

let fail e : JSON<_> =
    fun json ->
        Choice2Of2 e, json

let getM : JSON<_> =
    fun json ->
        Choice1Of2 json, json

let setM json : JSON<_> =
    fun _ ->
        Choice1Of2 (), json

let modM f : JSON<_> =
    fun json ->
        Choice1Of2 (), f json