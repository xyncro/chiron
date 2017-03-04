module ChironB.Benchmarks.Program

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Analysers
open BenchmarkDotNet.Diagnosers
//open BenchmarkDotNet.Diagnostics.Windows
open BenchmarkDotNet.Validators
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    // BenchmarkRunner.Run<ChironPerformance.ParsingAndFormatting.ParseSmallPayload>(
    //     ManualConfig
    //         .Create(DefaultConfig.Instance)
    //         .With(ExecutionValidator.FailOnError)
    //         .With(JitOptimizationsValidator.FailOnError))
    // let switcher = BenchmarkSwitcher thisAssembly
    // let _ = switcher.Run argv
    //let x = BenchmarkRunner.Run<Chiron.Benchmarks.ParseSmallPayload>(BenchmarkDotNet.Configs.DefaultConfig.Instance.KeepBenchmarkFiles)
    let _ = BenchmarkRunner.Run<FormatTest>()
    let _ = BenchmarkRunner.Run<ParseTest>()
    0
