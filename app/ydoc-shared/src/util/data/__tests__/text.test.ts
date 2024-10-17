import { fc, test } from '@fast-check/vitest'
import { expect } from 'vitest'
import { applyTextEdits, applyTextEditsToSpans, textChangeToEdits, trimEnd } from '../text'

test.prop({
  before: fc.array(fc.boolean(), { minLength: 32, maxLength: 64 }),
  after: fc.array(fc.boolean(), { minLength: 32, maxLength: 64 }),
})('textChangeToEdits / applyTextEdits round-trip', ({ before, after }) => {
  // Generate strings composed of a mix of only two characters so that `textChangeToEdits` will find a variety of
  // similarities between the inputs.
  const stringFromBools = (bools: Array<boolean>) => bools.map(bool => (bool ? 't' : 'f')).join('')
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
  const leadingSpacesAndLength = (input: string): [number, number] => [
    input.lastIndexOf(' ') + 1,
    input.length,
  ]
  const spacesAndHyphens = ([spaces, length]: readonly [number, number]) => {
    return ' '.repeat(spaces) + '-'.repeat(length - spaces)
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
