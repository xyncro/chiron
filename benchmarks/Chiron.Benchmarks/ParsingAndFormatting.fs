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

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old () : ChironObsolete.Json =
        ChironObsolete.Parsing.Json.parse jsonString

    [<Benchmark>]
    member __.Chiron_New () : Chiron.JsonResult<Chiron.Json> =
        Chiron.Parsing.Json.parse jsonString

[<Config(typeof<CoreConfig>)>]
type FormatTest () =
    let mutable jsonN = Chiron.Json.Null

    let mutable jsonO = ChironObsolete.Json.Null ()

    [<Setup>]
    member this.Setup () =
        jsonN <-
            loadJsonResourceAsString this.Name
            |> Chiron.Parsing.Json.parse
            |> Chiron.JsonResult.getOrThrow
        jsonO <-
            loadJsonResourceAsString this.Name
            |> ChironObsolete.Parsing.Json.parse

    [<Params("error", "fparsec", "user", "prettyuser", "social")>]
    member val Name = "<null>" with get, set

    [<Benchmark>]
    member __.Chiron_New () =
        Chiron.Formatting.Json.format jsonN

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old () =
        ChironObsolete.Formatting.Json.format jsonO

[<Config(typeof<CoreConfig>)>]
type FormatVariableLengthStrings () =
    let mutable simpleJson = Chiron.Json.Null
    let mutable escapedJson = Chiron.Json.Null
    let mutable simpleJsonO = ChironObsolete.Json.Null ()
    let mutable escapedJsonO = ChironObsolete.Json.Null ()

    [<Params(10, 100, 1000, 10000, 100000)>]
    member val public strlen = 1 with get, set

    [<Setup>]
    member x.Setup () =
        let simple = String.replicate x.strlen "a"
        simpleJson <- Chiron.Json.String simple
        simpleJsonO <- ChironObsolete.Json.String simple
        let escaped = String.replicate (x.strlen / 10) "\\u0004\\n\\\""
        escapedJson <- Chiron.Json.String escaped
        escapedJsonO <- ChironObsolete.Json.String escaped

    [<Benchmark>]
    member __.Simple_New () =
        Chiron.Formatting.Json.format simpleJson

    [<Benchmark>]
    member __.Escaped_New () =
        Chiron.Formatting.Json.format escapedJson

    [<Benchmark>]
    member __.Simple_Old () =
        ChironObsolete.Formatting.Json.format simpleJsonO

    [<Benchmark>]
    member __.Escaped_Old () =
        ChironObsolete.Formatting.Json.format escapedJsonO
