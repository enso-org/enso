import { expect, test } from 'vitest'
import { escapeColumn } from '../completionData'

test.each([
  { name: 'A Column' },
  { name: '[A Column With Brackets]', escaped: '[A Column With Brackets]]' },
  { name: '[[A Column With Nested Brackets]]', escaped: '[[A Column With Nested Brackets]]]]' },
  { name: 'Brackets [in the] middle', escaped: 'Brackets [in the]] middle' },
  { name: 'Nested brackets [[in the]] middle', escaped: 'Nested brackets [[in the]]]] middle' },
  { name: 'With an [ open bracket' },
  { name: 'With a ] close bracket', escaped: 'With a ]] close bracket' },
])('Column name escaping', ({ name, escaped }) => expect(escapeColumn(name)).toBe(escaped ?? name))
