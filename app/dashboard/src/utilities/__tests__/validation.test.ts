/** @file Basic tests for this */
import * as v from 'vitest'

import * as validation from '#/utilities/validation'

// =============
// === Tests ===
// =============

/** Runs all tests. */
v.test('password validation', () => {
  const pattern = new RegExp(`^(?:${validation.PASSWORD_PATTERN})$`)
  const emptyPassword = ''
  v.expect(emptyPassword, `'${emptyPassword}' fails validation`).not.toMatch(pattern)
  const shortPassword = 'Aa0!'
  v.expect(shortPassword, `'${shortPassword}' is too short`).not.toMatch(pattern)
  const passwordMissingDigit = 'Aa!Aa!Aa!'
  v.expect(passwordMissingDigit, `'${passwordMissingDigit}' is missing a digit`).not.toMatch(
    pattern
  )
  const passwordMissingLowercase = 'A0!A0!A0!'
  v.expect(
    passwordMissingLowercase,
    `'${passwordMissingLowercase}' is missing a lowercase letter`
  ).not.toMatch(pattern)
  const passwordMissingUppercase = 'a0!a0!a0!'
  v.expect(
    passwordMissingUppercase,
    `'${passwordMissingUppercase}' is missing an uppercase letter`
  ).not.toMatch(pattern)
  const passwordMissingSymbol = 'Aa0Aa0Aa0'
  v.expect(passwordMissingSymbol, `'${passwordMissingSymbol}' is missing a symbol`).not.toMatch(
    pattern
  )
  const validPassword = 'Aa0!Aa0!'
  v.expect(validPassword, `'${validPassword}' passes validation`).toMatch(pattern)
  const basicPassword = 'Password0!'
  v.expect(basicPassword, `'${basicPassword}' passes validation`).toMatch(pattern)
  const issue7498Password = 'ÑéFÛÅÐåÒ.ú¿¼\u00b4N@aö¶U¹jÙÇ3'
  v.expect(issue7498Password, `'${issue7498Password}' passes validation`).toMatch(pattern)
})
