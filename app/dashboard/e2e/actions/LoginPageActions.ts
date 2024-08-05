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
  loginThatShouldFail(email = VALID_EMAIL, password = VALID_PASSWORD, error?: string) {
    const next = this.step('Login (should fail)', () => this.loginInternal(email, password))
    if (error == null) {
      return next
    } else {
      return next.step(`Expect error to be '${error}'`, async (page) => {
        await test.expect(page.getByTestId('form-submit-error')).toHaveText(error)
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
