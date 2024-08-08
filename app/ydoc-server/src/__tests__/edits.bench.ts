import diff from 'fast-diff'
import { uuidv4 } from 'lib0/random.js'
import { bench, describe } from 'vitest'
import { stupidFastDiff } from '../edits'

describe('Diff algorithm benchmarks', () => {
  let oldString = ''
  let newString = ''

  function initializeStrings(length: number) {
    // simulating metadata list:
    oldString = ''
    let i = 0
    while (oldString.length < length / 2) {
      oldString += `[{"index":{"value":${i}},"size":{"value":4}},"${uuidv4()}"]`
      i += 1
    }
    i = 1
    newString = ''
    while (newString.length < length / 2) {
      newString += `[{"index":{"value":${i}},"size":{"value":5}},"${uuidv4()}"]`
    }
  }

  function diffBenchmark(length: number) {
    bench(
      `Diffing ${length}`,
      () => {
        diff(oldString, newString)
      },
      { setup: () => initializeStrings(length), warmupIterations: 1, iterations: 1 },
    )
  }

  function stupidFastDiffBenchmark(length: number) {
    bench(
      `Fast Diffing ${length}`,
      () => {
        stupidFastDiff(oldString, newString)
      },
      { setup: () => initializeStrings(length), warmupIterations: 1, iterations: 1 },
    )
  }

  diffBenchmark(10000)
  diffBenchmark(20000)
  diffBenchmark(30000)
  diffBenchmark(40000)
  diffBenchmark(50000)
  // These are too slow for every-day benchmark run
  //   diffBenchmark(60000)
  //   diffBenchmark(70000)
  //   diffBenchmark(80000)
  //   diffBenchmark(90000)
  //   diffBenchmark(100000)
  //   diffBenchmark(250000)

  stupidFastDiffBenchmark(250000)
  stupidFastDiffBenchmark(2500000)
})
