/** @file Tests for `error.ts`. */
import * as v from 'vitest'

import * as errorModule from '../error'

// =============
// === Tests ===
// =============

const MESSAGE = 'A custom error message.'

v.test.each([
    { error: new Error(MESSAGE), message: MESSAGE },
    { error: { message: 'a' }, message: 'a' },
    { error: MESSAGE, message: null },
    { error: {}, message: null },
    { error: null, message: null },
])('`error.tryGetMessage`', ({ error, message }) => {
    v.expect(errorModule.tryGetMessage<unknown>(error)).toBe(message)
})
