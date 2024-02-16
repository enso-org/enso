import { expect, test } from 'vitest'
import { applyTextEditsToSpans, textChangeToEdits, trimEnd } from '../text'

/** Tests that:
 *  - When the code in `a[0]` is edited to become the code in `b[0]`,
 *    `applyTextEditsToSpans` followed by `trimEnd` transforms the spans in `a.slice(1)` into the spans in `b.slice(1)`.
 *  - The same holds when editing from `b` to `a`.
 */
function checkCorrespondence(a: string[], b: string[]) {
  checkCorrespondenceForward(a, b)
  checkCorrespondenceForward(b, a)
}

/** Performs the same check as {@link checkCorrespondence}, for correspondences that are not expected to be reversible.
 */
function checkCorrespondenceForward(before: string[], after: string[]) {
  const leadingSpacesAndLength = (input: string): [number, number] => [
    input.lastIndexOf(' ') + 1,
    input.length,
  ]
  const spacesAndHyphens = ([spaces, length]: readonly [number, number]) => {
    let s = ''
    for (let i = 0; i < spaces; i++) s += ' '
    for (let i = spaces; i < length; i++) s += '-'
    return s
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
