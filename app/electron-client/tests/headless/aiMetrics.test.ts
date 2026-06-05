/** @file Unit tests for the AI-effectiveness telemetry helpers. */
import type { RequestUsage } from 'enso-common/src/ai'
import { promises as fs } from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { appendMetricsRow, parseAiUsageLine, sanitizeForFilename } from '../aiMetrics'

function makeSample(overrides: Partial<RequestUsage> = {}): RequestUsage {
  return {
    inputTokens: 1000,
    outputTokens: 500,
    cacheReadTokens: 10_000,
    cacheCreationTokens: 200,
    contextTokens: 25_000,
    contextFromLastHop: true,
    hopCount: 1,
    durationMs: 1_500,
    ...overrides,
  }
}

describe('parseAiUsageLine', () => {
  test('parses a clean line with ctxSrc=lastHop and no fresh marker', () => {
    const line =
      '[AI] usage: prompt=1000t out=500t context=25.0k hops=1 ctxSrc=lastHop (cacheRead=10000t cacheCreate=200t) time=1500ms'
    expect(parseAiUsageLine(line)).toEqual({
      inputTokens: 1000,
      outputTokens: 500,
      contextTokens: 25_000,
      hopCount: 1,
      contextFromLastHop: true,
      freshAgent: false,
      cacheReadTokens: 10_000,
      cacheCreationTokens: 200,
      durationMs: 1500,
    })
  })

  test('flips contextFromLastHop to false on ctxSrc=fallback', () => {
    const line =
      '[AI] usage: prompt=10t out=20t context=0.1k hops=2 ctxSrc=fallback (cacheRead=0t cacheCreate=0t) time=10ms'
    expect(parseAiUsageLine(line)?.contextFromLastHop).toBe(false)
  })

  test('flips freshAgent to true when the bare `fresh` marker is present', () => {
    const line =
      '[AI] usage: prompt=10t out=20t context=0.1k hops=1 ctxSrc=lastHop fresh (cacheRead=0t cacheCreate=0t) time=10ms'
    expect(parseAiUsageLine(line)?.freshAgent).toBe(true)
  })

  test('returns null for an unrelated console line', () => {
    expect(parseAiUsageLine('Loading Enso…')).toBeNull()
  })
})

describe('sanitizeForFilename', () => {
  test('lowercases and collapses non-alphanumeric runs into single dashes', () => {
    expect(sanitizeForFilename('Week 32 — Pokemon Card Organising')).toBe(
      'week-32-pokemon-card-organising.csv',
    )
  })

  test('trims leading/trailing dashes', () => {
    expect(sanitizeForFilename('—abc—')).toBe('abc.csv')
  })

  test('falls back to _.csv for all-non-alphanumeric titles', () => {
    expect(sanitizeForFilename('!!!')).toBe('_.csv')
  })
})

describe('appendMetricsRow', () => {
  let tmpDir: string

  beforeEach(async () => {
    tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'aimetrics-test-'))
  })
  afterEach(async () => {
    await fs.rm(tmpDir, { recursive: true, force: true })
  })

  async function readCsv(testName: string): Promise<string[]> {
    const csvPath = path.join(tmpDir, sanitizeForFilename(testName))
    const content = await fs.readFile(csvPath, 'utf8')
    // Trailing newline yields an empty last element; drop it for cleaner assertions.
    return content.replace(/\n$/, '').split('\n')
  }

  test('writes the header on first invocation, plain row on subsequent ones', async () => {
    const args = {
      dir: tmpDir,
      testName: 't',
      samples: [makeSample()],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'pass' as const,
    }
    await appendMetricsRow(args)
    let lines = await readCsv('t')
    expect(lines).toHaveLength(2)
    expect(lines[0]).toMatch(/^timestamp,commit,ai_parameters,status,test_name,/)

    await appendMetricsRow(args)
    lines = await readCsv('t')
    expect(lines).toHaveLength(3)
    // Second invocation does NOT re-emit the header.
    expect(lines.filter((l) => l.startsWith('timestamp,'))).toHaveLength(1)
  })

  test('status is the bare outcome when all samples have lastHop context', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [makeSample(), makeSample({ hopCount: 3 })],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'pass',
    })
    const [, row] = await readCsv('t')
    expect(row!.split(',')[3]).toBe('pass')
  })

  test('status gets the `(broken)` suffix when a multi-hop sample fell back to cost-side context', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [makeSample(), makeSample({ contextFromLastHop: false, hopCount: 2 })],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'pass',
    })
    const [, row] = await readCsv('t')
    expect(row!.split(',')[3]).toBe('pass (broken)')
  })

  test('a fallback sample with hopCount=0 does NOT trigger the (broken) suffix', async () => {
    // hopCount=0 is the degenerate "no assistant envelope observed" case — there's no last
    // hop to be missing, so the fallback isn't a sign of broken telemetry.
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [makeSample({ contextFromLastHop: false, hopCount: 0 })],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'fail',
    })
    const [, row] = await readCsv('t')
    expect(row!.split(',')[3]).toBe('fail')
  })

  test('outcome=fail with broken telemetry composes to `fail (broken)`', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [makeSample({ contextFromLastHop: false, hopCount: 2 })],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'fail',
    })
    const [, row] = await readCsv('t')
    expect(row!.split(',')[3]).toBe('fail (broken)')
  })

  test('ai_parameters is recorded verbatim, including embedded commas (CSV-escaped)', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [makeSample()],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '--model claude-sonnet-4-6, --foo bar',
      outcome: 'pass',
    })
    const [, row] = await readCsv('t')
    // CSV-escape wraps any value containing `,` in quotes — column index 2 is `ai_parameters`.
    expect(row).toContain('"--model claude-sonnet-4-6, --foo bar"')
  })

  test('totals and per-node columns aggregate across samples', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [
        makeSample({ durationMs: 100, hopCount: 1, inputTokens: 10 }),
        makeSample({ durationMs: 200, hopCount: 2, inputTokens: 20 }),
      ],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'pass',
    })
    const [, row] = await readCsv('t')
    const cells = row!.split(',')
    // Columns: timestamp, commit, ai_parameters, status, test_name, node_count,
    //   total_duration_ms, total_hops, total_input_tokens, …
    expect(cells[5]).toBe('2') // node_count
    expect(cells[6]).toBe('300') // total_duration_ms
    expect(cells[7]).toBe('3') // total_hops
    expect(cells[8]).toBe('30') // total_input_tokens
    // per_node arrays are semicolon-joined; per_node_durations_ms is index 13.
    expect(cells[13]).toBe('100;200')
  })

  test('empty samples produces a zero-count row, no per-node values', async () => {
    await appendMetricsRow({
      dir: tmpDir,
      testName: 't',
      samples: [],
      commit: 'abc',
      timestamp: '2026-01-01T00:00:00.000Z',
      aiParameters: '',
      outcome: 'fail',
    })
    const [, row] = await readCsv('t')
    const cells = row!.split(',')
    expect(cells[3]).toBe('fail') // status — no samples means no broken signal
    expect(cells[5]).toBe('0') // node_count
    expect(cells[12]).toBe('0') // final_context_tokens defaults to 0 for empty samples
    expect(cells[13]).toBe('') // per_node_durations_ms is empty when there are no samples
  })
})
