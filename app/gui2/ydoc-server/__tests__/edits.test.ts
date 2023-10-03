import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'
import * as Y from 'yjs'
import { applyDiffAsTextEdits, applyTextEdits, convertDeltaToTextEdits } from '../edits'
describe('applyDiffAsTextEdits', () => {
  test('no change', () => {
    const edits = applyDiffAsTextEdits(0, 'abcd', 'abcd')
    expect(edits).toStrictEqual([])
  })
  test('simple add', () => {
    const before = 'abcd'
    const after = 'abefcd'
    const edits = applyDiffAsTextEdits(1, before, after)
    expect(edits).toStrictEqual([
      {
        range: { end: { character: 2, line: 1 }, start: { character: 2, line: 1 } },
        text: 'ef',
      },
    ])
  })
  test('two adds', () => {
    const before = 'abcd'
    const after = 'abefcdxy'
    const edits = applyDiffAsTextEdits(1, before, after)
    expect(edits).toStrictEqual([
      {
        range: { end: { character: 2, line: 1 }, start: { character: 2, line: 1 } },
        text: 'ef',
      },
      {
        range: { end: { character: 6, line: 1 }, start: { character: 6, line: 1 } },
        text: 'xy',
      },
    ])
  })

  test.prop({
    linesBefore: fc.array(fc.string(), { minLength: 1 }),
    linesAfter: fc.array(fc.string(), { minLength: 1 }),
    ctx: fc.context(),
  })('should correctly create edits from random diffs', ({ linesBefore, linesAfter, ctx }) => {
    const before = linesBefore.join('\n')
    const after = linesAfter.join('\n')
    const edits = applyDiffAsTextEdits(0, before, after)
    for (const edit of edits) {
      ctx.log(
        `${edit.range.start.line}:${edit.range.start.character} - ` +
          `${edit.range.end.line}:${edit.range.end.character} - '${edit.text}'`,
      )
    }
    const applied = applyTextEdits(before, edits)
    expect(applied).toBe(after)
  })
})

describe('convertDeltaToTextEdits', () => {
  function genDeltaAndResultFromOps(
    initial: string,
    ops: ({ insert: [number, string] } | { delete: [number, number] })[],
  ): { delta: Y.YTextEvent['delta']; result: string } {
    const doc = new Y.Doc()
    const ytext = doc.getText()
    ytext.insert(0, initial)
    let delta: Y.YTextEvent['delta'] | null = null
    ytext.observe((e) => {
      delta = e.delta
    })

    doc.transact(() => {
      for (const op of ops) {
        if ('insert' in op) {
          ytext.insert(Math.min(op.insert[0], ytext.length), op.insert[1])
        } else {
          const idx = Math.min(op.delete[0], ytext.length)
          const len = Math.min(op.delete[1], ytext.length - idx)
          ytext.delete(idx, len)
        }
      }
    })

    return {
      delta: delta ?? [],
      result: ytext.toString(),
    }
  }

  function deltaOp() {
    return fc.oneof(
      fc.record({ insert: fc.tuple(fc.nat(), fc.string()) }),
      fc.record({ delete: fc.tuple(fc.nat(), fc.nat()) }),
    )
  }

  test.prop({
    initialLines: fc.array(fc.string()),
    ops: fc.array(deltaOp()),
    deltaCtx: fc.context(),
    ctx: fc.context(),
  })(
    'should correctly translate deltas to random diffs',
    ({ initialLines, ops, deltaCtx, ctx }) => {
      const initial = initialLines.join('\n')
      const { delta, result } = genDeltaAndResultFromOps(initial, ops)
      for (const op of delta) {
        if ('retain' in op) {
          deltaCtx.log(`retain ${op.retain}`)
        } else if ('delete' in op) {
          deltaCtx.log(`delete ${op.delete}`)
        } else {
          deltaCtx.log(`insert ${op.insert}`)
        }
      }
      const { code, edits } = convertDeltaToTextEdits(initial, delta)
      for (const edit of edits) {
        ctx.log(
          `${edit.range.start.line}:${edit.range.start.character} - ` +
            `${edit.range.end.line}:${edit.range.end.character} - '${edit.text}'`,
        )
      }
      const applied = applyTextEdits(initial, edits)
      expect(applied).toBe(result)
      expect(code).toBe(result)
    },
  )
})
