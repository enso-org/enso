
import { describe, expect, it } from 'vitest'
import { parseLinesFromJobOutput } from '../src/utils'

describe('Parsing from job output', () => {
    it('should parse a final line from JMH', () => {
        const line = "2024-06-12T02:04:09.5753331Z  INFO ide_ci::program::command: sbtℹ️ [0m[[0m[0minfo[0m] [0m[0mo.e.compiler.benchmarks.inline.InlineCompilerBenchmark.longExpression                                     avgt    4    2.008 ±  0.811  ms/op[0m"
        const results = parseLinesFromJobOutput([line])
        expect(results.size).toEqual(1)
        expect(results.get("o.e.compiler.benchmarks.inline.InlineCompilerBenchmark.longExpression")).toBe(2.008)
    })
})