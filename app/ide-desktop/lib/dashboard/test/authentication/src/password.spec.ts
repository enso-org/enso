/** @file Basic tests for this */
import * as test from '@playwright/test'

import * as validation from '../../../src/authentication/src/dashboard/validation'

/** Runs all tests. */
test.test('password validation', () => {
    const pattern = new RegExp(`^(?:${validation.PASSWORD_PATTERN})$`)
    const emptyPassword = ''
    test.expect(!pattern.test(emptyPassword), `'${emptyPassword}' fails validation`)
    const shortPassword = 'Aa0!'
    test.expect(!pattern.test(shortPassword), `'${shortPassword}' is too short`)
    const passwordMissingDigit = 'Aa!Aa!Aa!'
    test.expect(!pattern.test(passwordMissingDigit), `'${passwordMissingDigit}' is missing a digit`)
    const passwordMissingLowercase = 'A0!A0!A0!'
    test.expect(
        !pattern.test(passwordMissingLowercase),
        `'${passwordMissingLowercase}' is missing a lowercase letter`
    )
    const passwordMissingUppercase = 'a0!a0!a0!'
    test.expect(
        !pattern.test(passwordMissingUppercase),
        `'${passwordMissingUppercase}' is missing an uppercase letter`
    )
    const passwordMissingSymbol = 'Aa0Aa0Aa0'
    test.expect(
        !pattern.test(passwordMissingSymbol),
        `'${passwordMissingSymbol}' is missing a symbol`
    )
    const validPassword = 'Aa0!Aa0!'
    test.expect(pattern.test(validPassword), `'${validPassword}' passes validation`)
    const basicPassword = 'Password0!'
    test.expect(pattern.test(basicPassword), `'${basicPassword}' passes validation`)
    const issue7498Password = 'ÑéFÛÅÐåÒ.ú¿¼\u00b4N@aö¶U¹jÙÇ3'
    test.expect(pattern.test(issue7498Password), `'${issue7498Password}' passes validation`)
})
