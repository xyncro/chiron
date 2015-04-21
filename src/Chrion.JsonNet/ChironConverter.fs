namespace Chrion.JsonNet

open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Chiron

type ChironConverter() =
  inherit JsonConverter()

  override x.CanConvert(typ:Type) = 
    let toJson = typ.GetMethod("ToJson")
    let fromJson = typ.GetMethod("FromJson")
    toJson <> null && fromJson <> null
    && toJson.IsStatic && toJson.IsPublic
    && fromJson.IsStatic && toJson.IsPublic
    && (let ps = toJson.GetParameters() in ps.Length = 1 && ps.[0].ParameterType = typ)
    && (let ps = fromJson.GetParameters() in ps.Length = 1 && ps.[0].ParameterType = typ)
    && toJson.ReturnType.GetGenericTypeDefinition() = typedefof<Json<_>>
    && fromJson.ReturnType.GetGenericTypeDefinition() = typedefof<Json<_>>

  override x.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
    writer.WriteRaw(Json.serialize value |> Json.format)

  override x.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
    let json = JObject.Load(reader).ToString()
    Json.parse json |> Json.deserialize |> box