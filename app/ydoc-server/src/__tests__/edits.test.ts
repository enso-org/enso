import { fc, test } from '@fast-check/vitest'
import { describe, expect } from 'vitest'
import { Position, TextEdit } from 'ydoc-shared/languageServerTypes'
import { applyDiffAsTextEdits, stupidFastDiff } from '../edits'

// ======================
// === Test utilities ===
// ======================

/**
 * Apply text edits intended for language server to a given starting text. Used for verification
 * during testing if generated edits were correct. This is intentionally a very simple, not
 * performant implementation.
 */
export function applyTextEdits(content: string, edits: TextEdit[]): string {
  return edits.reduce((c, edit) => {
    try {
      const startOffset = lsPositionToTextOffset(c, edit.range.start)
      const endOffset = lsPositionToTextOffset(c, edit.range.end)
      return c.slice(0, startOffset) + edit.text + c.slice(endOffset)
    } catch (e) {
      throw new Error(
        `Failed to apply edit ${JSON.stringify(edit)} to content:\n${JSON.stringify(c)}\n${e}`,
      )
    }
  }, content)
}

function lsPositionToTextOffset(content: string, pos: Position): number {
  const lineData = getNthLineLengthAndOffset(content, pos.line)
  if (pos.character > lineData.length)
    throw new Error(
      `Character ${pos.character} is out of bounds for line ${pos.line}. ` +
        `Line length ${lineData.length}.`,
    )
  return lineData.offset + pos.character
}

function getNthLineLengthAndOffset(content: string, line: number) {
  const nthLineRegex = new RegExp(`((?:.*\\n){${line}})(.*)(?:\n|$)`)
  const match = nthLineRegex.exec(content)
  if (!match) throw new Error(`Line ${line} not found in content:\n${content}`)
  return {
    offset: match[1]!.length,
    length: match[2]!.length,
  }
}

// ==================
// === Test suite ===
// ==================

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

test.each`
  oldStr     | newStr      | expected
  ${''}      | ${'foo'}    | ${[[1, 'foo']]}
  ${'foo'}   | ${''}       | ${[[-1, 'foo']]}
  ${'foo'}   | ${'foo'}    | ${[[0, 'foo']]}
  ${'foo'}   | ${'bar'}    | ${[[-1, 'foo'], [1, 'bar']]}
  ${'ababx'} | ${'acacx'}  | ${[[0, 'a'], [-1, 'bab'], [1, 'cac'], [0, 'x']]}
  ${'ax'}    | ${'acacx'}  | ${[[0, 'a'], [1, 'cac'], [0, 'x']]}
  ${'ababx'} | ${'ax'}     | ${[[0, 'a'], [-1, 'bab'], [0, 'x']]}
  ${'ababx'} | ${'abacax'} | ${[[0, 'aba'], [-1, 'b'], [1, 'ca'], [0, 'x']]}
  ${'axxxa'} | ${'a'}      | ${[[0, 'a'], [-1, 'xxxa']]}
`('Stupid diff of $oldStr and $newStr', ({ oldStr, newStr, expected }) => {
  expect(stupidFastDiff(oldStr, newStr)).toEqual(expected)
})
