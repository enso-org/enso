/** @file Tests for `error.ts`. */
import * as v from 'vitest'

import * as error from '../error'

// =============
// === Tests ===
// =============

v.test('tryGetMessage', () => {
    const message = 'A custom error message.'
    v.expect(error.tryGetMessage<unknown>(new Error(message))).toBe(message)
    v.expect(error.tryGetMessage<unknown>({ message: 'a' })).toBe('a')
    v.expect(error.tryGetMessage<unknown>(message)).toBeNull()
    v.expect(error.tryGetMessage<unknown>({})).toBeNull()
    v.expect(error.tryGetMessage<unknown>(null)).toBeNull()
})
