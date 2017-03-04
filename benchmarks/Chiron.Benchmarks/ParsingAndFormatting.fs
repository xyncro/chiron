namespace ChironB.Benchmarks

open Chiron
open BenchmarkDotNet.Attributes
open Newtonsoft.Json

[<Config(typeof<CoreConfig>)>]
type ParseVariableLengthStrings () =
    let mutable simpleStr = ""
    let mutable escapedStr = ""

    [<Params(10, 100, 1000, 10000, 100000)>]
    member val public strlen = 1 with get, set

    [<Setup>]
    member x.Setup () =
        let simple = String.replicate x.strlen "a"
        simpleStr <- "\"" + simple + "\""
        let escaped = String.replicate (x.strlen / 10) "\\u0004\\n\\\""
        escapedStr <- "\"" + escaped + "\""

    [<Benchmark>]
    member __.Simple () =
        Json.parse simpleStr

    [<Benchmark>]
    member __.Escaped () =
        Json.parse escapedStr

type ErrorMsg =
    { error: System.Guid; message: string }
    static member FromJson (_:ErrorMsg) = json {
        let! error = Json.read "errorId"
        let! message = Json.read "message"
        return { error = error; message = message }
    }

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

        let inline parseAndDeserialize (stream : #Stream) : 'a =
            let reader = new StreamReader(stream)
            reader.ReadToEnd()
            |> Json.parse
            |> Json.deserialize

    module JsonNET =
        open Newtonsoft.Json

        let serializer = JsonSerializer.CreateDefault()

        let inline deserialize<'a> (stream : #Stream) : 'a =
            let jsonReader = new StreamReader(stream, Encoding.UTF8)
            let reader = new JsonTextReader(jsonReader, CloseInput = false)
            serializer.Deserialize<'a> reader

[<Config(typeof<CoreConfig>)>]
type ParseTest () =
    [<Benchmark>]
    member __.Chiron_Old_Parse () : Json =
        let jsonString = loadJsonResourceAsString "fparsec"
        Chiron.Parsing.Json.parse jsonString

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old_ParseStream () : Json =
        use jsonStream = loadJsonResource "fparsec"
        Bench.Chiron.parse jsonStream

[<Config(typeof<CoreConfig>)>]
type FormatTest () =
    let jsonO =
        loadJsonResourceAsString "fparsec"
        |> Chiron.Parsing.Json.parse

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old () =
        Chiron.Formatting.Json.format jsonO

type ParseSmallPayload () =
    let jsonStream : System.IO.Stream = loadJsonResource "error"

    interface System.IDisposable with
        member __.Dispose () =
            if not (isNull jsonStream) then jsonStream.Dispose()

    [<Benchmark>]
    member __.Chiron_Deserialize () : ErrorMsg =
        Bench.resetStream jsonStream
        Bench.Chiron.parseAndDeserialize jsonStream

    [<Benchmark>]
    member __.Chiron_ParseOnly () : Json =
        Bench.resetStream jsonStream
        Bench.Chiron.parse jsonStream

    [<Benchmark>]
    member __.JsonNET_Deserialize () : ErrorMsg =
        Bench.resetStream jsonStream
        Bench.JsonNET.deserialize jsonStream

[<Config(typeof<CoreConfig>)>]
type ParseMediumPayload () =
    let jsonStream : System.IO.Stream = loadJsonResource "user"

    interface System.IDisposable with
        member __.Dispose () =
            if not (isNull jsonStream) then jsonStream.Dispose()

    // [<Benchmark>]
    // member __.Chiron_Deserialize () : ErrorMsg =
    //     Bench.resetStream jsonStream
    //     Bench.Chiron.parseAndDeserialize jsonStream

    [<Benchmark>]
    member __.Chiron_ParseOnly () : Json =
        Bench.resetStream jsonStream
        Bench.Chiron.parse jsonStream

    // [<Benchmark>]
    // member __.JsonNET_Deserialize () : User =
    //     Bench.resetStream jsonStream
    //     Bench.JsonNET.deserialize jsonStream

[<Config(typeof<CoreConfig>)>]
type ParseMediumPrettyPayload () =
    let jsonStream : System.IO.Stream = loadJsonResource "prettyuser"

    interface System.IDisposable with
        member __.Dispose () =
            if not (isNull jsonStream) then jsonStream.Dispose()

    // [<Benchmark>]
    // member __.Chiron_Deserialize () : User =
    //     Bench.resetStream jsonStream
    //     Bench.Chiron.parseAndDeserialize jsonStream

    [<Benchmark>]
    member __.Chiron_ParseOnly () : Json =
        Bench.resetStream jsonStream
        Bench.Chiron.parse jsonStream

    // [<Benchmark>]
    // member __.JsonNET_Deserialize () : User =
    //     Bench.resetStream jsonStream
    //     Bench.JsonNET.deserialize jsonStream

[<Config(typeof<CoreConfig>)>]
type ParseLargePayload () =
    let jsonStream : System.IO.Stream = loadJsonResource "social"

    interface System.IDisposable with
        member __.Dispose () =
            if not (isNull jsonStream) then jsonStream.Dispose()

    // [<Benchmark>]
    // member __.Chiron_Deserialize () : Social =
    //     Bench.resetStream jsonStream
    //     Bench.Chiron.parseAndDeserialize jsonStream

    [<Benchmark>]
    member __.Chiron_ParseOnly () : Json =
        Bench.resetStream jsonStream
        Bench.Chiron.parse jsonStream

    // [<Benchmark>]
    // member __.JsonNET_Deserialize () : Social =
    //     Bench.resetStream jsonStream
    //     Bench.JsonNET.deserialize jsonStream

[<Config(typeof<CoreConfig>)>]
type FormatVariableLengthStrings () =
    let mutable simpleJson = Json.Null ()
    let mutable escapedJson = Json.Null ()

    [<Params(10, 100, 1000, 10000, 100000)>]
    member val public strlen = 1 with get, set

    [<Setup>]
    member x.Setup () =
        let simple = String.replicate x.strlen "a"
        simpleJson <- Json.String simple
        let escaped = String.replicate (x.strlen / 10) "\\u0004\\n\\\""
        escapedJson <- Json.String escaped

    [<Benchmark>]
    member __.Simple () =
        Json.format simpleJson

    [<Benchmark>]
    member __.Escaped () =
        Json.format escapedJson
