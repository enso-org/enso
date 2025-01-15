//
// Demo script showing a "serialization" of an Enso atom structure
// to a `.hprof` file which can be opened and analyzed in VisualVM
//
// Based on following tutorial:
// https://www.graalvm.org/jdk21/tools/graalvm-insight/manual/#heap-dumping
//
// Execute from `sbt` as:
// ```
// sbt:enso> runEngineDistribution \
//    --vm.D=polyglot.heap.dump=/tmp/sieve.hprof \
//    --vm.D=polyglot.insight=Benchmarks/src/Sieve/heap_dump_lazy_sieve.js \
//    --run test/Benchmarks/src/Sieve/Lazy_Sieve.enso
//    10000
// ```
//
// This GraalVM script waits for the end execution of `compute_and_print_nth` function
// to store value of its `p` local variable (containing linked list of prime numbers)
// into `/tmp/sieve.hprof` - a GraalVM serialization for Truffle languages!

insight.on(
  'return',
  (ctx, frame) => {
    for (let p in frame) {
      print(`found ${p} with ${frame[p]} value`)
    }
    let value = frame.p
    heap.dump({
      format: '1.0',
      depth: 5000,
      events: [
        {
          stack: [
            {
              at: ctx,
              frame: {
                primes: value,
              },
            },
          ],
        },
      ],
    })
    print('Heap dump generated!')
  },
  {
    roots: true,
    rootNameFilter: '.*compute_and_print_nth',
  },
)
