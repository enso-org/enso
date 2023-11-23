/** @file Tests for `error.ts`. */
import * as test from '@playwright/test'

import * as error from '../../../src/authentication/src/error'

// =============
// === Tests ===
// =============

test.test('tryGetMessage', () => {
    const message = 'A custom error message.'
    test.expect(error.tryGetMessage<unknown>(new Error(message))).toBe(message)
    test.expect(error.tryGetMessage<unknown>(message)).toBeNull()
    test.expect(error.tryGetMessage<unknown>({})).toBeNull()
    test.expect(error.tryGetMessage<unknown>(null)).toBeNull()
})
