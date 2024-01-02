/** @file Tests for `error.ts`. */
import * as error from '../../../src/utilities/error'
import * as test from '@playwright/test'

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
