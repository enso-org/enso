/** @file Basic tests for this */
import * as v from 'vitest'

import * as validation from '#/utilities/validation'

// =============
// === Tests ===
// =============

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

v.test.each([
  { name: 'foo', valid: true },
  { name: 'foo/', valid: false },
  { name: 'foo\\', valid: false },
  { name: 'foo/bar', valid: false },
  { name: 'foo\\bar', valid: false },
  { name: '/bar', valid: false },
  { name: '\\bar', valid: false },
  { name: '\\', valid: false },
  { name: '/', valid: false },
  { name: '......', valid: false },
  { name: '..', valid: false },
  { name: '.', valid: true },
  { name: 'a.a.a.a.a.a.a.a.', valid: true },
  { name: 'a.a.a.a.a.a.a.a.a', valid: true },
  { name: '.a.a.a.a.a.a.a.a', valid: true },
  { name: 'a.a.a.a.a.a.a.a..', valid: false },
  { name: './', valid: false },
  { name: '//', valid: false },
  { name: '/\\', valid: false },
  { name: '\\/', valid: false },
])('directory name validation', (args) => {
  const { name, valid } = args
  const regex = validation.DIRECTORY_NAME_REGEX
  if (valid) {
    v.expect(name, `'${name}' is a valid directory name`).toMatch(regex)
  } else {
    v.expect(name, `'${name}' is not a valid directory name`).not.toMatch(regex)
  }
})
