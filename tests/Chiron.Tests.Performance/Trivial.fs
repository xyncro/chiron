module Trivial

open NBench
open NBench.Util

let inline getCounter (ctx : BenchmarkContext) str = ctx.GetCounter(str)
let inline incr (ctr : Counter) = ctr.Increment()
let inline decr (ctr : Counter) = ctr.Decrement()

type BenchmarkFixture() =
    let mutable counter : Counter = Unchecked.defaultof<_>

    [<PerfSetup>]
    member __.Setup (ctx : BenchmarkContext) : unit =
        counter <- getCounter ctx "TestCounter"

    [<PerfBenchmark(
        Description = "Test",
        NumberOfIterations = 3,
        RunMode = RunMode.Throughput,
        RunTimeMilliseconds = 1000,
        TestMode = TestMode.Test)>]
    [<CounterThroughputAssertion("TestCounter", MustBe.GreaterThan, 10000000.)>]
    member __.Benchmark (ctx : BenchmarkContext) : unit =
        incr counter