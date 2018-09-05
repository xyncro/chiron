namespace ChironB.Benchmarks

open Chiron
open BenchmarkDotNet.Attributes
open Newtonsoft.Json

module Bench =
    open System.IO
    open System.Text

    let resetStream (stream : #Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore

    module Chiron =
        let inline parse (stream : #Stream) : Json =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> Json.parse
            |> JsonResult.getOrThrow

        // let inline parseAndDeserialize (stream : #Stream) : 'a =
        //     let reader = new StreamReader(stream)
        //     reader.ReadToEnd()
        //     |> Json.parse
        //     |> Json.deserialize

    module JsonNET =
        open Newtonsoft.Json

        let serializer = JsonSerializer.CreateDefault()

        let inline deserialize<'a> (stream : #Stream) : 'a =
            let jsonReader = new StreamReader(stream, Encoding.UTF8)
            let reader = new JsonTextReader(jsonReader, CloseInput = false)
            serializer.Deserialize<'a> reader

[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    let mutable jsonString = "<null>"

    [<Setup>]
    member this.Setup () =
        jsonString <- loadJsonResourceAsString this.Name

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.Chiron_New () : Chiron.JsonResult<Chiron.Json> =
        Chiron.Parsing.Json.parse jsonString

[<Config(typeof<CoreConfig>)>]
type FormatTest () =
    let mutable jsonN = Chiron.Json.Null

    [<Setup>]
    member this.Setup () =
        jsonN <-
            loadJsonResourceAsString this.Name
            |> Chiron.Parsing.Json.parse
            |> Chiron.JsonResult.getOrThrow

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.Chiron_New () =
        Chiron.Formatting.Json.format jsonN

[<Config(typeof<CoreConfig>)>]
type FormatVariableLengthStrings () =
    let mutable simpleJson = Chiron.Json.Null
    let mutable escapedJson = Chiron.Json.Null

    [<Params(10, 100, 1000, 10000, 100000)>]
    member val public strlen = 1 with get, set

    [<Setup>]
    member x.Setup () =
        let simple = String.replicate x.strlen "a"
        simpleJson <- Chiron.Json.String simple
        let escaped = String.replicate (x.strlen / 10) "\\u0004\\n\\\""
        escapedJson <- Chiron.Json.String escaped

    [<Benchmark>]
    member __.Simple_New () =
        Chiron.Formatting.Json.format simpleJson

    [<Benchmark>]
    member __.Escaped_New () =
        Chiron.Formatting.Json.format escapedJson
