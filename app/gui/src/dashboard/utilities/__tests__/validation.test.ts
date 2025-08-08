/** @file Basic tests for this */
import * as v from 'vitest'

import * as validation from '#/utilities/validation'

/** Runs all tests. */
v.test('password validation', () => {
  const regex = validation.PASSWORD_REGEX
  const emptyPassword = ''
  v.expect(emptyPassword, `'${emptyPassword}' fails validation`).not.toMatch(regex)
  const shortPassword = 'Aa0!'
  v.expect(shortPassword, `'${shortPassword}' is too short`).not.toMatch(regex)
  const passwordMissingDigit = 'Aa!Aa!Aa!'
  v.expect(passwordMissingDigit, `'${passwordMissingDigit}' is missing a digit`).not.toMatch(regex)
  const passwordMissingLowercase = 'A0!A0!A0!'
  v.expect(
    passwordMissingLowercase,
    `'${passwordMissingLowercase}' is missing a lowercase letter`,
  ).not.toMatch(regex)
  const passwordMissingUppercase = 'a0!a0!a0!'
  v.expect(
    passwordMissingUppercase,
    `'${passwordMissingUppercase}' is missing an uppercase letter`,
  ).not.toMatch(regex)
  const passwordMissingSymbol = 'Aa0Aa0Aa0'
  v.expect(passwordMissingSymbol, `'${passwordMissingSymbol}' is missing a symbol`).not.toMatch(
    regex,
  )
  const validPassword = 'Aa0!Aa0!'
  v.expect(validPassword, `'${validPassword}' passes validation`).toMatch(regex)
  const basicPassword = 'Password0!'
  v.expect(basicPassword, `'${basicPassword}' passes validation`).toMatch(regex)
  const issue7498Password = 'ÑéFÛÅÐåÒ.ú¿¼\u00b4N@aö¶U¹jÙÇ3'
  v.expect(issue7498Password, `'${issue7498Password}' passes validation`).toMatch(regex)
})
