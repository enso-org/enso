import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'
import {
  type SourceRange,
  applyTextEdits,
  applyTextEditsToSpans,
  rangeEncloses,
  rangeIntersects,
  textChangeToEdits,
  trimEnd,
} from '../text'

type RangeTest = { a: SourceRange; b: SourceRange }

function rangeTest({ a, b }: { a: number[]; b: number[] }) {
  return { a: { from: a[0]!, to: a[1]! }, b: { from: b[0]!, to: b[1]! } }
}

const equalRanges: RangeTest[] = [
  { a: [0, 0], b: [0, 0] },
  { a: [0, 1], b: [0, 1] },
  { a: [-5, 5], b: [-5, 5] },
].map(rangeTest)

const totalOverlap: RangeTest[] = [
  { a: [0, 1], b: [0, 0] },
  { a: [0, 2], b: [2, 2] },
  { a: [-1, 1], b: [1, 1] },
  { a: [0, 2], b: [0, 1] },
  { a: [-10, 10], b: [-3, 7] },
  { a: [0, 5], b: [1, 2] },
  { a: [3, 5], b: [3, 4] },
].map(rangeTest)

const reverseTotalOverlap: RangeTest[] = totalOverlap.map(({ a, b }) => ({ a: b, b: a }))

const noOverlap: RangeTest[] = [
  { a: [0, 1], b: [2, 3] },
  { a: [0, 1], b: [-1, -1] },
  { a: [5, 6], b: [2, 3] },
  { a: [0, 2], b: [-2, -1] },
  { a: [-5, -3], b: [9, 10] },
  { a: [-3, 2], b: [3, 4] },
].map(rangeTest)

const partialOverlap: RangeTest[] = [
  { a: [0, 3], b: [-1, 1] },
  { a: [0, 1], b: [-1, 0] },
  { a: [0, 0], b: [-1, 0] },
  { a: [0, 2], b: [1, 4] },
  { a: [-8, 0], b: [0, 10] },
].map(rangeTest)

test.each([...equalRanges, ...totalOverlap])('Range $a should enclose $b', ({ a, b }) =>
  expect(rangeEncloses(a, b)).toBe(true),
)
test.each([...noOverlap, ...partialOverlap, ...reverseTotalOverlap])(
  'Range $a should not enclose $b',
  ({ a, b }) => expect(rangeEncloses(a, b)).toBe(false),
)
test.each([...equalRanges, ...totalOverlap, ...reverseTotalOverlap, ...partialOverlap])(
  'Range $a should intersect $b',
  ({ a, b }) => expect(rangeIntersects(a, b)).toBe(true),
)
test.each([...noOverlap])('Range $a should not intersect $b', ({ a, b }) =>
  expect(rangeIntersects(a, b)).toBe(false),
)

test.prop({
  before: fc.array(fc.boolean(), { minLength: 32, maxLength: 64 }),
  after: fc.array(fc.boolean(), { minLength: 32, maxLength: 64 }),
})('textChangeToEdits / applyTextEdits round-trip', ({ before, after }) => {
  // Generate strings composed of a mix of only two characters so that `textChangeToEdits` will find a variety of
  // similarities between the inputs.
  const stringFromBools = (bools: Array<boolean>) =>
    bools.map((bool) => (bool ? 't' : 'f')).join('')
  const beforeString = stringFromBools(before)
  const afterString = stringFromBools(after)
  const edits = textChangeToEdits(beforeString, afterString)
  expect(applyTextEdits(beforeString, edits)).toBe(afterString)
})

/**
 * Test that `textChangeToEdits` and `applyTextEdits` work when inputs contain any special characters representable by
 *  a `string`, including newlines and even incomplete surrogate pairs (invalid Unicode).
 */
test.prop({
  before: fc.array(fc.string16bits(), { maxLength: 8 }),
})('textChangeToEdits / applyTextEdits round-trip: Special characters', ({ before }) => {
  const beforeString = before.join('\n')
  // Produce the after-string by rearranging the lines of the before-string, so that the edit-relationship between them
  // is non-trivial.
  const afterString = before.sort().join('\n')
  const edits = textChangeToEdits(beforeString, afterString)
  expect(applyTextEdits(beforeString, edits)).toBe(afterString)
})

/**
 * Tests that:
 *  - When the code in `a[0]` is edited to become the code in `b[0]`,
 *    `applyTextEditsToSpans` followed by `trimEnd` transforms the spans in `a.slice(1)` into the spans in `b.slice(1)`.
 *  - The same holds when editing from `b` to `a`.
 */
function checkCorrespondence(a: string[], b: string[]) {
  checkCorrespondenceForward(a, b)
  checkCorrespondenceForward(b, a)
}

/**
  Performs the same check as {@link checkCorrespondence}, for correspondences that are not expected to be reversible.
 */
function checkCorrespondenceForward(before: string[], after: string[]) {
  const leadingSpacesAndLength = (input: string): SourceRange => ({
    from: input.lastIndexOf(' ') + 1,
    to: input.length,
  })
  const spacesAndHyphens = ({ from, to }: SourceRange) => {
    return ' '.repeat(from) + '-'.repeat(to - from)
  }
  const edits = textChangeToEdits(before[0]!, after[0]!)
  const spansAfter = applyTextEditsToSpans(edits, before.slice(1).map(leadingSpacesAndLength)).map(
    ([_spanBefore, spanAfter]) => trimEnd(spanAfter, after[0]!),
  )
  expect([after[0]!, ...spansAfter.map(spacesAndHyphens)]).toEqual(after)
}

test('applyTextEditsToSpans: Add and remove argument names.', () => {
  checkCorrespondence(
    [
      'func arg1 arg2', // prettier-ignore
      '----',
      '     ----',
      '---------',
      '          ----',
      '--------------',
    ],
    [
      'func name1=arg1 name2=arg2',
      '----',
      '           ----',
      '---------------',
      '                      ----',
      '--------------------------',
    ],
  )
})

test('applyTextEditsToSpans: Lengthen and shorten argument names.', () => {
  checkCorrespondence(
    [
      'func name1=arg1 name2=arg2',
      '----',
      '           ----',
      '---------------',
      '                      ----',
      '--------------------------',
    ],
    [
      'func longName1=arg1 longName2=arg2',
      '----',
      '               ----',
      '-------------------',
      '                              ----',
      '----------------------------------',
    ],
  )
})

test('applyTextEditsToSpans: Add and remove inner application.', () => {
  checkCorrespondence(
    [
      'func bbb2', // prettier-ignore
      '----',
      '     ----',
      '---------',
    ],
    [
      'func aaa1 bbb2', // prettier-ignore
      '----',
      '          ----',
      '--------------',
    ],
  )
})

test('applyTextEditsToSpans: Add and remove outer application.', () => {
  checkCorrespondence(
    [
      'func arg1', // prettier-ignore
      '----',
      '     ----',
      '---------',
    ],
    [
      'func arg1 arg2', // prettier-ignore
      '----',
      '     ----',
      '---------',
    ],
  )
})

test('applyTextEditsToSpans: Distinguishing repeated subexpressions.', () => {
  checkCorrespondence(
    [
      'foo (2 + 2) bar () (2 + 2)', // prettier-ignore
      '     -----',
      '    -------',
      '                    -----',
      '                   -------',
    ],
    [
      'foo (2 + 2) bar (2 + 2) (2 + 2)', // prettier-ignore
      '     -----',
      '    -------',
      '                         -----',
      '                        -------',
    ],
  )
})

test('applyTextEditsToSpans: Space after line content.', () => {
  checkCorrespondenceForward(
    [
      'value = 1 +', // prettier-ignore
      '-----------',
    ],
    [
      'value = 1 ', // prettier-ignore
      '---------',
    ],
  )
})
