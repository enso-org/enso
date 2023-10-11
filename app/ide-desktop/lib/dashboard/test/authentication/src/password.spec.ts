/** @file Basic tests for this */
import * as test from '@playwright/test'

import * as validation from '../../../src/authentication/src/dashboard/validation'

// =============
// === Tests ===
// =============

/** Runs all tests. */
test.test('password validation', () => {
    const pattern = new RegExp(`^(?:${validation.PASSWORD_PATTERN})$`)
    const emptyPassword = ''
    test.expect(emptyPassword, `'${emptyPassword}' fails validation`).not.toMatch(pattern)
    const shortPassword = 'Aa0!'
    test.expect(shortPassword, `'${shortPassword}' is too short`).not.toMatch(pattern)
    const passwordMissingDigit = 'Aa!Aa!Aa!'
    test.expect(passwordMissingDigit, `'${passwordMissingDigit}' is missing a digit`).not.toMatch(
        pattern
    )
    const passwordMissingLowercase = 'A0!A0!A0!'
    test.expect(
        passwordMissingLowercase,
        `'${passwordMissingLowercase}' is missing a lowercase letter`
    ).not.toMatch(pattern)
    const passwordMissingUppercase = 'a0!a0!a0!'
    test.expect(
        passwordMissingUppercase,
        `'${passwordMissingUppercase}' is missing an uppercase letter`
    ).not.toMatch(pattern)
    const passwordMissingSymbol = 'Aa0Aa0Aa0'
    test.expect(
        passwordMissingSymbol,
        `'${passwordMissingSymbol}' is missing a symbol`
    ).not.toMatch(pattern)
    const validPassword = 'Aa0!Aa0!'
    test.expect(validPassword, `'${validPassword}' passes validation`).toMatch(pattern)
    const basicPassword = 'Password0!'
    test.expect(basicPassword, `'${basicPassword}' passes validation`).toMatch(pattern)
    const issue7498Password = 'ÑéFÛÅÐåÒ.ú¿¼\u00b4N@aö¶U¹jÙÇ3'
    test.expect(issue7498Password, `'${issue7498Password}' passes validation`).toMatch(pattern)
})
