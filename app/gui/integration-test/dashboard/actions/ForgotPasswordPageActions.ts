/** @file Available actions for the login page. */
import { expect } from '@playwright/test'

import { TEXT, VALID_EMAIL } from '.'
import BaseActions, { type LocatorCallback } from './BaseActions'
import LoginPageActions from './LoginPageActions'

/** Available actions for the login page. */
export default class ForgotPasswordPageActions<Context> extends BaseActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage() {
    return {
      login: (): LoginPageActions<Context> =>
        this.step("Go to 'login' page", async (page) =>
          page.getByRole('link', { name: TEXT.goBackToLogin, exact: true }).click(),
        ).into(LoginPageActions<Context>),
    }
  }

  /** Perform a successful login. */
  forgotPassword(email = VALID_EMAIL) {
    return this.step('Forgot password', () => this.forgotPasswordInternal(email)).into(
      LoginPageActions<Context>,
    )
  }

  /** Fill the email input. */
  fillEmail(email: string) {
    return this.step(`Fill email with '${email}'`, (page) =>
      page.getByPlaceholder(TEXT.emailPlaceholder).fill(email),
    )
  }

  /** Interact with the email input. */
  withEmailInput(callback: LocatorCallback<Context>) {
    return this.step('Interact with email input', async (page, context) => {
      await callback(page.getByPlaceholder(TEXT.emailPlaceholder), context)
    })
  }

  /** Internal login logic shared between all public methods. */
  private async forgotPasswordInternal(email: string) {
    await this.page.getByPlaceholder(TEXT.emailPlaceholder).fill(email)
    await this.page
      .getByRole('button', { name: TEXT.login, exact: true })
      .getByText(TEXT.login)
      .click()
    await expect(this.page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
  }
}
