/** @file Unit tests for the tolerant JSON extractor in claudeAgent.ts. */
import { describe, expect, test, vi } from 'vitest'

vi.mock('cross-spawn', () => ({ default: vi.fn() }))
vi.mock('electron', () => ({ ipcMain: { handle: vi.fn(), on: vi.fn() } }))

const { extractJsonObject } = await import('../../src/claudeAgent')

describe('extractJsonObject', () => {
  test('parses a clean JSON object directly', () => {
    expect(extractJsonObject('{"functionName":"foo","argumentNames":[]}')).toEqual({
      functionName: 'foo',
      argumentNames: [],
    })
  })

  test('extracts the object when the model emits narration before the JSON', () => {
    const text = 'Drafting filter step\n{"functionName":"foo","argumentNames":["table"]}'
    expect(extractJsonObject(text)).toEqual({
      functionName: 'foo',
      argumentNames: ['table'],
    })
  })

  test('extracts the object when narration follows the JSON', () => {
    const text = '{"functionName":"foo"}\nDone.'
    expect(extractJsonObject(text)).toEqual({ functionName: 'foo' })
  })

  test('handles braces inside string literals without closing the object early', () => {
    const text = '{"body":"x = { not = json }\\nx"}'
    expect(extractJsonObject(text)).toEqual({ body: 'x = { not = json }\nx' })
  })

  test('handles escaped quotes inside string literals', () => {
    const text = '{"body":"say \\"hi\\""}'
    expect(extractJsonObject(text)).toEqual({ body: 'say "hi"' })
  })

  test('picks the last balanced object when multiple appear', () => {
    const text = '{"a":1} stray text {"b":2}'
    expect(extractJsonObject(text)).toEqual({ b: 2 })
  })

  test('returns null when no balanced object is present', () => {
    expect(extractJsonObject('just narration with no json at all')).toBeNull()
  })

  test('returns null when only an unbalanced opening brace is present', () => {
    expect(extractJsonObject('starts here { but never closes')).toBeNull()
  })

  test('returns null for a top-level scalar JSON value with no embedded object', () => {
    // The fast path accepts only objects (`typeof === 'object'` excludes scalars), and the
    // brace-scan fallback finds no `{...}`.
    expect(extractJsonObject('42')).toBeNull()
    expect(extractJsonObject('"hello"')).toBeNull()
  })
})
