open Chiron
open Chiron.Operators

type Test =
    { Text: string
      Number: float option
      Sub: SubTest }

    static member FromJson (_: Test) =
            fun t n s ->
                { Text = t
                  Number = n
                  Sub = s }
        <!> Json.read "text"
        <*> Json.tryRead "number"
        <*> Json.read "sub"

and SubTest =
    { SubText: string }

    static member FromJson (_: SubTest) =
            fun st ->
                { SubText = st }
        <!> Json.read "subText"


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

    printfn "%s" test.Text

    0
