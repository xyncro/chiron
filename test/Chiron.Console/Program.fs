open Chiron
open Chiron.Operators

type Test =
    { Text: string
      Number: float
      Sub: SubTest }

    static member FromJson (_: Test) =
            fun t n s ->
                { Text = t
                  Number = n
                  Sub = s }
        <!> Json.read "text"
        <*> Json.read "number"
        <*> Json.read "sub"

    static member ToJson (x: Test) =
            Json.write "text" x.Text
         *> Json.write "number" x.Number
         *> Json.write "sub" x.Sub

and SubTest =
    { SubText: string }

    static member FromJson (_: SubTest) =
            fun st ->
                { SubText = st }
        <!> Json.read "subText"

    static member ToJson (x: SubTest) =
        Json.write "subText" x.SubText

let jsonSample =
    """{
      "text": "hello world",
      "number": 21.5,
      "sub": {
        "subText": "foo"
      }
    }"""

[<EntryPoint>]
let main _ =


    let json = Json.parse jsonSample
    let test = Json.deserialize json

    let json2 = Json.serialize test

    printfn "%s" test.Text

    0
