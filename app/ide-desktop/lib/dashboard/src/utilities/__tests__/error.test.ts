/** @file Tests for `error.ts`. */
import * as v from 'vitest'

import * as error from '#/utilities/error'

// =============
// === Tests ===
// =============

const MESSAGE = 'A custom error message.'

v.test.each([
  { errorObject: new Error(MESSAGE), message: MESSAGE },
  { errorObject: { message: 'a' }, message: 'a' },
  { errorObject: MESSAGE, message: MESSAGE },
  { errorObject: {}, message: String({}) },
  { errorObject: null, message: String(null) },
])('`error.getMessageOrToString`', ({ errorObject, message }) => {
  v.expect(error.getMessageOrToString<unknown>(errorObject)).toBe(message)
})
