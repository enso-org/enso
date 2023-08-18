/** @file Basic tests for this */
import chalk from 'chalk'

import * as validation from './src/authentication/src/dashboard/validation'

// =================
// === Constants ===
// =================

/** The text displayed on success. */
const SUCCESS_TEXT = chalk.green('✔') + ' '
/** The text displayed on failure. */
const FAILURE_TEXT = chalk.red('✗') + ' '

// ==================
// === TestRunner ===
// ==================

/** A simple test runner. */
class TestRunner {
    succeeded = 0
    failed = 0
    total = 0

    /** Prints a success or error message depending on whether the `condition` is true. */
    expect(condition: boolean, message: string) {
        this.total += 1
        if (condition) {
            this.succeeded += 1
        } else {
            this.failed += 1
        }
        const prefix = condition ? SUCCESS_TEXT : FAILURE_TEXT
        console.log(`${prefix} ${message}`)
    }

    /** Prints a summary of test results. */
    summarize() {
        console.log(
            `\n${chalk.green(this.succeeded)}/${this.total} tests succeeded, ${chalk.red(
                this.failed
            )}/${this.total} tests failed` +
                (this.failed === 0 ? '\n' + chalk.green('All tests passed') : '')
        )
    }
}

// =============
// === Tests ===
// =============

/** Runs all tests. */
function runAllTests() {
    const test = new TestRunner()
    const pattern = new RegExp(`^(?:${validation.PASSWORD_PATTERN})$`)
    const emptyPassword = ''
    test.expect(
        !pattern.test(emptyPassword),
        `${chalk.yellow(`'${emptyPassword}'`)} fails validation`
    )
    const shortPassword = 'Aa0!'
    test.expect(!pattern.test(shortPassword), `${chalk.yellow(`'${shortPassword}'`)} is too short`)
    const passwordMissingDigit = 'Aa!Aa!Aa!'
    test.expect(
        !pattern.test(passwordMissingDigit),
        `${chalk.yellow(`'${passwordMissingDigit}'`)} is missing a digit`
    )
    const passwordMissingLowercase = 'A0!A0!A0!'
    test.expect(
        !pattern.test(passwordMissingLowercase),
        `${chalk.yellow(`'${passwordMissingLowercase}'`)} is missing a lowercase letter`
    )
    const passwordMissingUppercase = 'a0!a0!a0!'
    test.expect(
        !pattern.test(passwordMissingUppercase),
        `${chalk.yellow(`'${passwordMissingUppercase}'`)} is missing an uppercase letter`
    )
    const passwordMissingSymbol = 'Aa0Aa0Aa0'
    test.expect(
        !pattern.test(passwordMissingSymbol),
        `${chalk.yellow(`'${passwordMissingSymbol}'`)} is missing a symbol`
    )
    const validPassword = 'Aa0!Aa0!'
    test.expect(
        pattern.test(validPassword),
        `${chalk.yellow(`'${validPassword}'`)} passes validation`
    )
    const basicPassword = 'Password0!'
    test.expect(
        pattern.test(basicPassword),
        `${chalk.yellow(`'${basicPassword}'`)} passes validation`
    )
    const issue7498Password = 'ÑéFÛÅÐåÒ.ú¿¼\u00b4N@aö¶U¹jÙÇ3'
    test.expect(
        pattern.test(issue7498Password),
        `${chalk.yellow(`'${issue7498Password}'`)} passes validation`
    )
    test.summarize()
}

runAllTests()
