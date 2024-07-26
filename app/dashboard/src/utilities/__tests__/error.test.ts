/** @file Tests for `error.ts`. */
import * as v from 'vitest'

import * as error from '#/utilities/error'

// =============
// === Tests ===
// =============

const MESSAGE = 'A custom error message.'

v.describe('`error.tryGetMessage`', () => {
  v.test.each([
    { errorObject: new Error(MESSAGE), message: MESSAGE },
    { errorObject: { message: 'a' }, message: 'a' },
    { errorObject: MESSAGE, message: null },
    { errorObject: {}, message: null },
    { errorObject: null, message: null },
  ])('should correctly parse error objects', ({ errorObject, message }) => {
    v.expect(error.tryGetMessage<unknown>(errorObject)).toBe(message)
  })

  v.test.each([
    { errorObject: new Error(MESSAGE), message: MESSAGE, defaultMessage: 'b' },
    { errorObject: { message: 'a' }, message: 'a', defaultMessage: 'b' },
    { errorObject: MESSAGE, message: 'b', defaultMessage: 'b' },
    { errorObject: {}, message: 'b', defaultMessage: 'b' },
    { errorObject: null, message: 'b', defaultMessage: 'b' },
  ])(
    'should return default message if failed to extract it from the error object',
    ({ errorObject, message, defaultMessage }) => {
      v.expect(error.tryGetMessage<unknown, string>(errorObject, defaultMessage)).toBe(message)
    },
  )
})
