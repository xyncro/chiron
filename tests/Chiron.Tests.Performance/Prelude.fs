[<AutoOpen>]
module internal Prelude

open NBench

let inline getCounter (ctx : BenchmarkContext) str = ctx.GetCounter(str)
let inline incr (ctr : Counter) = ctr.Increment()
let inline decr (ctr : Counter) = ctr.Decrement()
