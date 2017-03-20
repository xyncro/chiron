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

    [<Benchmark>]
    member __.Chiron_New_Parse () : ChironN.NewParser.Json =
        let jsonString = loadJsonResourceAsString "fparsec"
        ChironN.NewParser.Json.parse jsonString

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old_ParseStream () : Json =
        use jsonStream = loadJsonResource "fparsec"
        Bench.Chiron.parse jsonStream

    [<Benchmark>]
    member __.Chiron_New_ParseStream () : ChironN.NewParser.Result<ChironN.NewParser.Json,_> =
        use jsonStream = loadJsonResource "fparsec"
        ChironN.NewParser.Json.parseStream jsonStream

[<Config(typeof<CoreConfig>)>]
type FormatTest () =
    let mutable jsonN =
        loadJsonResourceAsString "fparsec"
        |> ChironN.NewParser.Json.parse

    let mutable jsonO =
        loadJsonResourceAsString "fparsec"
        |> Chiron.Parsing.Json.parse

    let ms = System.IO.MemoryStream()
    let sb = System.Text.StringBuilder()

    [<Setup>]
    member this.Setup () =
        jsonN <-
            loadJsonResourceAsString this.Name
            |> ChironN.NewParser.Json.parse
        jsonO <-
            loadJsonResourceAsString this.Name
            |> Chiron.Parsing.Json.parse

    [<Params("error", "fparsec", "social")>]
    member val Name = "fparsec" with get, set

    [<Benchmark>]
    member __.Chiron_New () =
        ChironN.NewParser.Formatting.Json.format jsonN

    [<Benchmark>]
    member __.Chiron_New_Pre () =
        let sb = System.Text.StringBuilder(ChironN.NewParser.Formatting.calcLength 0 ChironN.NewParser.Formatting.JsonFormattingOptions.Compact 0 jsonN)
        ChironN.NewParser.Formatting.formatJson sb ChironN.NewParser.Formatting.JsonFormattingOptions.Compact 0 jsonN
        sb.ToString()

    [<Benchmark>]
    member __.Chiron_New_ToStringWriter () =
        sb.Clear()
        use sw = System.IO.StringWriter(sb)
        ChironN.NewParser.Formatting.Stream.Json.formatInto sw jsonN
        sw.Flush()

    // [<Benchmark>]
    // member __.Chiron_New_ToStringWriterThenceToString () =
    //     sb.Clear()
    //     use sw = System.IO.StringWriter(sb)
    //     ChironN.NewParser.Formatting.Stream.Json.formatInto sw jsonN
    //     sw.Flush()
    //     sb.ToString()

    // [<Benchmark>]
    // member __.Chiron_New_PrecalcStringBuilderSizeToStringWriterThenceToString () =
    //     let sb = System.Text.StringBuilder(ChironN.NewParser.Formatting.calcLength 0u ChironN.NewParser.Formatting.JsonFormattingOptions.Compact 0u jsonN |> int)
    //     use sw = System.IO.StringWriter(sb)
    //     ChironN.NewParser.Formatting.Stream.Json.formatInto sw jsonN
    //     sw.Flush()
    //     sb.ToString()

    // [<Benchmark>]
    // member __.Chiron_New_TaskToStringWriter () =
    //     sb.Clear()
    //     use sw = System.IO.StringWriter(sb)
    //     let t = ChironN.NewParser.Formatting.Stream.Json.formatIntoAsTask sw jsonN
    //     t.Wait()
    //     sw.Flush()

    // [<Benchmark>]
    // member __.Chiron_New_AsyncToStringWriter () =
    //     sb.Clear()
    //     use sw = System.IO.StringWriter(sb)
    //     ChironN.NewParser.Formatting.Stream.Json.formatIntoAsAsync sw jsonN
    //     |> Async.RunSynchronously
    //     sw.Flush()

    [<Benchmark>]
    member __.Chiron_New_ToStreamWriter () =
        ms.Seek(0L, System.IO.SeekOrigin.Begin)
        let sw = System.IO.StreamWriter(ms)
        ChironN.NewParser.Formatting.Stream.Json.formatInto sw jsonN
        sw.Flush()

    // [<Benchmark>]
    // member __.Chiron_New_TaskToStreamWriter () =
    //     ms.Seek(0L, System.IO.SeekOrigin.Begin)
    //     let sw = System.IO.StreamWriter(ms)
    //     let t = ChironN.NewParser.Formatting.Stream.Json.formatIntoAsTask sw jsonN
    //     t.Wait()
    //     sw.Flush()

    // [<Benchmark>]
    // member __.Chiron_New_AsyncToStreamWriter () =
    //     ms.Seek(0L, System.IO.SeekOrigin.Begin)
    //     let sw = System.IO.StreamWriter(ms)
    //     ChironN.NewParser.Formatting.Stream.Json.formatIntoAsAsync sw jsonN
    //     |> Async.RunSynchronously
    //     sw.Flush()

    [<Benchmark(Baseline=true)>]
    member __.Chiron_Old () =
        Chiron.Formatting.Json.format jsonO

type MyType =
    { prop: string
      prop2: string }
    static member ToJson (x: MyType) = Chiron.Builder.json {
        do! Chiron.Mapping.Json.write "prop" x.prop
        do! Chiron.Mapping.Json.write "prop2" x.prop2
    }
    static member Serialize (x: MyType) =
        ChironN.NewParser.JsonObject.empty
        |> ChironN.NewParser.JsonObject.add ("prop", ChironN.NewParser.SerializationDefaults.Serialize x.prop)
        |> ChironN.NewParser.JsonObject.add ("prop2", ChironN.NewParser.SerializationDefaults.Serialize x.prop2)
        |> ChironN.NewParser.Json.Object

type MyType2 =
    { prop3: string
      prop4: string }
    static member Serialize (x: MyType2) =
        ChironN.NewParser.JsonObject.write ("prop3", x.prop3)
        >> ChironN.NewParser.JsonObject.write ("prop4", x.prop4)
        |> ChironN.NewParser.JsonObject.serializeAsObject

type MyType3 =
    { prop5: string
      prop6: string }
    static member Serialize : MyType3 -> ChironN.NewParser.Json = ChironN.NewParser.Serializer.Reflected.recordSerializerV2

[<Config(typeof<CoreConfig>)>]
type SerializeTest () =
    let myTypeDynSerializerV1 : ChironN.NewParser.Serializer<MyType> = ChironN.NewParser.Serializer.Reflected.recordSerializerV1
    let myTypeDynSerializerV2 : ChironN.NewParser.Serializer<MyType> = ChironN.NewParser.Serializer.Reflected.recordSerializerV2

    let example = { prop = "asdfasdfasdf"; prop2 = "f89asdhadsf'" }
    let example2 = { prop3 = "asdfasdfasdf"; prop4 = "f89asdhadsf'" }
    let example3 = { prop5 = "asdfasdfasdf"; prop6 = "f89asdhadsf'" }

    [<Benchmark(Baseline=true)>]
    member __.Chiron_New_Normal () =
        MyType.Serialize example

    [<Benchmark>]
    member __.Chiron_New_NormalWithHelper () =
        MyType2.Serialize example2

    [<Benchmark>]
    member __.Chiron_New_RuntimeGenerated_V1 () =
        myTypeDynSerializerV1 example

    [<Benchmark>]
    member __.Chiron_New_RuntimeGenerated_V2 () =
        myTypeDynSerializerV2 example

    [<Benchmark>]
    member __.Chiron_New_RuntimeGenerated_V2_2 () =
        MyType3.Serialize example3

    [<Benchmark>]
    member __.Chiron_Old () =
        MyType.ToJson example (Chiron.Json.Object Map.empty) |> ignore

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
