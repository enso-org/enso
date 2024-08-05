/** @file Available actions for the login page. */
import * as test from '@playwright/test'

import { TEXT, VALID_EMAIL, VALID_PASSWORD } from '../actions'
import BaseActions from './BaseActions'
import LoginPageActions from './LoginPageActions'

// ========================
// === LoginPageActions ===
// ========================

/** Available actions for the login page. */
export default class RegisterPageActions extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage() {
    return {
      login: (): LoginPageActions =>
        this.step("Go to 'login' page", async (page) =>
          page.getByRole('link', { name: TEXT.alreadyHaveAnAccount, exact: true }).click(),
        ).into(LoginPageActions),
    }
  }

  /** Perform a successful login. */
  register(email = VALID_EMAIL, password = VALID_PASSWORD, confirmPassword = password) {
    return this.step('Reegister', () =>
      this.registerInternal(email, password, confirmPassword),
    ).into(LoginPageActions)
  }

  /** Perform a failing login. */
  registerThatShouldFail(
    email = VALID_EMAIL,
    password = VALID_PASSWORD,
    confirmPassword = password,
    error?: string,
  ) {
    const next = this.step('Register (should fail)', () =>
      this.registerInternal(email, password, confirmPassword),
    )
    if (error == null) {
      return next
    } else {
      return next.step(`Expect error to be '${error}'`, async (page) => {
        await test.expect(page.getByTestId('form-submit-error')).toHaveText(error)
      })
    }
  }

  /** Internal login logic shared between all public methods. */
  private async registerInternal(email: string, password: string, confirmPassword: string) {
    await this.page.getByPlaceholder(TEXT.emailPlaceholder).fill(email)
    await this.page.getByPlaceholder(TEXT.passwordPlaceholder).fill(password)
    await this.page.getByPlaceholder(TEXT.confirmPasswordPlaceholder).fill(confirmPassword)
    await this.page
      .getByRole('button', { name: TEXT.register, exact: true })
      .getByText(TEXT.register)
      .click()
    await test.expect(this.page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
  }
}
