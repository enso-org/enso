/** @file Available actions for the login page. */
import * as test from '@playwright/test'

import { TEXT, VALID_EMAIL, VALID_PASSWORD } from '../actions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import RegisterPageActions from './RegisterPageActions'
import SetUsernamePageActions from './SetUsernamePageActions'

// ========================
// === LoginPageActions ===
// ========================

/** Available actions for the login page. */
export default class LoginPageActions extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage() {
    return {
      register: (): RegisterPageActions =>
        this.step("Go to 'register' page", async (page) =>
          page.getByRole('link', { name: TEXT.dontHaveAnAccount, exact: true }).click(),
        ).into(RegisterPageActions),
    }
  }

  /** Perform a successful login. */
  login(email = VALID_EMAIL, password = VALID_PASSWORD) {
    return this.step('Login', () => this.loginInternal(email, password)).into(DrivePageActions)
  }

  /** Perform a login as a new user (a user that does not yet have a username). */
  loginAsNewUser(email = VALID_EMAIL, password = VALID_PASSWORD) {
    return this.step('Login (as new user)', () => this.loginInternal(email, password)).into(
      SetUsernamePageActions,
    )
  }

  /** Perform a failing login. */
  loginThatShouldFail(
    email = VALID_EMAIL,
    password = VALID_PASSWORD,
    {
      assert = {},
    }: {
      assert?: {
        emailError?: string | null
        passwordError?: string | null
        formError?: string | null
      }
    } = {},
  ) {
    const { emailError, passwordError, formError } = assert
    const next = this.step('Login (should fail)', () => this.loginInternal(email, password))
      .expectInputError('email', 'email', emailError)
      .expectInputError('password', 'password', passwordError)
    if (formError === undefined) {
      return next
    } else if (formError != null) {
      return next.step(`Expect form error to be '${formError}'`, async (page) => {
        await test.expect(page.getByTestId('form-submit-error')).toHaveText(formError)
      })
    } else {
      return next.step('Expect no form error', async (page) => {
        await test.expect(page.getByTestId('form-submit-error')).not.toBeVisible()
      })
    }
  }

  /** Internal login logic shared between all public methods. */
  private async loginInternal(email: string, password: string) {
    await this.page.getByPlaceholder(TEXT.emailPlaceholder).fill(email)
    await this.page.getByPlaceholder(TEXT.passwordPlaceholder).fill(password)
    await this.page
      .getByRole('button', { name: TEXT.login, exact: true })
      .getByText(TEXT.login)
      .click()
    await test.expect(this.page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
  }
}
