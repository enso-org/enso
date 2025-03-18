import { expect, test } from 'vitest'
import { ExpressionTag, NestedChoiceTag } from '../tags'

test('Flattening nested choice tags', () => {
  const tag = new NestedChoiceTag('a', [
    new ExpressionTag('a'),
    new ExpressionTag('b'),
    new NestedChoiceTag('c', [new ExpressionTag('x'), new ExpressionTag('y')]),
  ])
  const flattened = tag.flatten()
  expect(flattened.map((t) => t.label)).toEqual(['a → a', 'a → b', 'a → c → x', 'a → c → y'])
})
