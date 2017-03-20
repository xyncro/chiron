[<AutoOpen>]
module internal Prelude

open System.Reflection
//open BenchmarkDotNet
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs
//open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Validators

type Dummy = Dummy

type CoreConfig() =
    inherit ManualConfig()
    do
        base.Add(Job.MediumRun)
        base.Add(EnvironmentAnalyser.Default)
        base.Add(MemoryDiagnoser.Default)
        base.Add(BaselineValidator.FailOnError)
        base.Add(JitOptimizationsValidator.FailOnError)

let thisAssembly = typeof<Dummy>.GetTypeInfo().Assembly

let loadJsonResource name =
        thisAssembly.GetManifestResourceStream(name + ".json")

let loadJsonResourceAsString name =
    use stream = loadJsonResource name
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd()
