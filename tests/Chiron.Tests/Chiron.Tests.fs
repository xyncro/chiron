module Chiron.Tests.Functional

open Aether
open Aether.Operators
open Chiron
open Chiron.Operators
open NUnit.Framework
open Swensen.Unquote

(* Cases *)

let private t1 =
    Object (Map.ofList
        [ "bool", Bool true
          "number", Number 2. ])

let private t2 =
    Object (Map.ofList
        [ "bool", Bool false
          "number", Number 2. ])

let private t3 =
    Object (Map.ofList
        [ "bool", Bool true
          "number", Number 4. ])

(* Functional

   Tests to exercise the basic functional components of the Json<'a>
   type and combinators thereof. *)

[<Test>]
let ``Json.init returns correct values`` () =
    Json.init 1 t1 =? (Value 1,  t1)

[<Test>]
let ``Json.error returns correct values`` () =
    Json.error "e" t1 =? (Error "e", t1)

[<Test>]
let ``Json.bind returns correct values`` () =
    Json.bind (Json.init 2) (fun x -> Json.init (x * 3)) t1 =? (Value 6, t1)
    Json.bind (Json.error "e") (fun x -> Json.init (x * 3)) t1 =? (Error "e", t1)

[<Test>]
let ``Json.apply returns correct values`` () =
    Json.apply (Json.init (fun x -> x * 3)) (Json.init 2) t1 =? (Value 6, t1)
    Json.apply (Json.init (fun x -> x * 3)) (Json.error "e") t1 =? (Error "e", t1)

[<Test>]
let ``Json.map returns correct values`` () =
    Json.map (fun x -> x * 3) (Json.init 2) t1 =? (Value 6, t1)
    Json.map (fun x -> x * 3) (Json.error "e") t1 =? (Error "e", t1)

[<Test>]
let ``Json.map2 returns correct values`` () =
    Json.map2 (*) (Json.init 2) (Json.init 3) t1 =? (Value 6, t1)
    Json.map2 (*) (Json.error "e") (Json.init 3) t1 =? (Error "e", t1)
    Json.map2 (*) (Json.init 2) (Json.error "e") t1 =? (Error "e", t1)

(* Lens

   Tests to exercise the functional lens based access to Json
   data structures. *)

let private lens =
         idLens 
    <-?> Json.ObjectPIso 
    >??> mapPLens "bool" 
    <??> Json.BoolPIso

[<Test>]
let ``Json.getLens returns correct values`` () =
    Json.getLens idLens t1 =? (Value t1, t1)

[<Test>]
let ``Json.getLensPartial returns correct values`` () =
    Json.getLensPartial lens t1 =? (Value true, t1)

[<Test>]
let ``Json.tryGetLensPartial returns correct values`` () =
    Json.tryGetLensPartial (idLens <-?> Json.NumberPIso) t1 =? (Value None, t1)

[<Test>]
let ``Json.setLens returns correct values`` () =
    Json.setLens idLens (Bool false) t1 =? (Value (), Bool false)

[<Test>]
let ``Json.setLensPartial returns correct values`` () =
    Json.setLensPartial lens false t1 =? (Value (), t2)

[<Test>]
let ``Json.mapLens returns correct values`` () =
    Json.mapLens idLens (fun _ -> Null ()) t1 =? (Value (), Null ())

[<Test>]
let ``Json.mapLensPartial returns correct values`` () =
    Json.mapLensPartial lens not t1 =? (Value (), t2)