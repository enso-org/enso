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
  { errorObject: MESSAGE, message: null },
  { errorObject: {}, message: null },
  { errorObject: null, message: null },
])('`error.tryGetMessage`', ({ errorObject, message }) => {
  v.expect(error.tryGetMessage<unknown>(errorObject)).toBe(message)
})
