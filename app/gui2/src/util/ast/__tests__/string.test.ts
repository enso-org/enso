import { escape } from '@/util/ast/string'
import { expect, test } from 'vitest'

test.each([
  { string: 'abcdef_123', escaped: 'abcdef_123' },
  { string: '\t\r\n\v"\'`', escaped: '\\t\\r\\n\\v\\"\\\'``' },
  { string: '`foo` `bar` `baz`', escaped: '``foo`` ``bar`` ``baz``' },
])('`escape`', ({ string, escaped }) => {
  expect(escape(string)).toBe(escaped)
})
