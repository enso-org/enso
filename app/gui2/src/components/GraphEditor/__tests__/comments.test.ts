import { cookedTextToRaw, rawTextToCooked } from '@/components/GraphEditor/GraphNodeComment.vue'
import { expect, test } from 'vitest'

const cases = [
  {
    raw: 'First paragraph\n\nSecond paragraph',
    cooked: 'First paragraph\nSecond paragraph',
  },
  {
    raw: 'First line\ncontinues on second line',
    cooked: 'First line continues on second line',
    normalized: 'First line continues on second line',
  },
]
test.each(cases)('Interpreting comments', ({ raw, cooked }) => {
  expect(rawTextToCooked(raw)).toBe(cooked)
})
test.each(cases)('Lowering comments', (testCase) => {
  const { raw, cooked } = testCase
  expect(cookedTextToRaw(cooked)).toBe(testCase?.normalized ?? raw)
})
