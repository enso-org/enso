/** @file Available actions for the login page. */
import { expect } from 'integration-test/base'
import BaseActions, { type LocatorCallback } from './BaseActions'
import LoginPageActions from './LoginPageActions'
import { TEXT, VALID_EMAIL, VALID_PASSWORD } from './utilities'

/** Available actions for the login page. */
export default class RegisterPageActions<Context> extends BaseActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage() {
    return {
      login: (): LoginPageActions<Context> =>
        this.step("Go to 'login' page", async (page) => {
          await page.getByRole('link', { name: TEXT.alreadyHaveAnAccount, exact: true }).click()
          await expect(page.getByText(TEXT.loginToYourAccount)).toBeVisible()
        }).into(LoginPageActions<Context>),
    }
  }

  /** Perform a successful login. */
  register(email = VALID_EMAIL, password = VALID_PASSWORD, confirmPassword = password) {
    return this.step('Reegister', () =>
      this.registerInternal(email, password, confirmPassword),
    ).into(LoginPageActions<Context>)
  }

  /** Perform a failing login. */
  registerThatShouldFail(
    email = VALID_EMAIL,
    password = VALID_PASSWORD,
    confirmPassword = password,
    {
      assert = {},
    }: {
      assert?: {
        emailError?: string | null
        passwordError?: string | null
        confirmPasswordError?: string | null
        formError?: string | null
      }
    } = {},
  ) {
    const { emailError, passwordError, confirmPasswordError, formError } = assert
    const next = this.step('Register (should fail)', () =>
      this.registerInternal(email, password, confirmPassword),
    )
      .expectInputError('email-input', 'email', emailError)
      .expectInputError('password-input', 'password', passwordError)
      .expectInputError('confirm-password-input', 'confirmPassword', confirmPasswordError)
    if (formError === undefined) {
      return next
    } else if (formError != null) {
      return next.step(`Expect form error to be '${formError}'`, async (page) => {
        await expect(page.getByTestId('form-submit-error')).toHaveText(formError)
      })
    } else {
      return next.step('Expect no form error', async (page) => {
        await expect(page.getByTestId('form-submit-error')).toBeHidden()
      })
    }
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
  private async registerInternal(email: string, password: string, confirmPassword: string) {
    await this.page.getByPlaceholder(TEXT.emailPlaceholder).fill(email)
    await this.page.getByPlaceholder(TEXT.passwordPlaceholder).fill(password)
    await this.page.getByPlaceholder(TEXT.confirmPasswordPlaceholder).fill(confirmPassword)
    await this.page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox)
      .click()
    await this.page
      .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
      .getByText(TEXT.privacyPolicyCheckbox)
      .click()
    await this.page
      .getByRole('button', { name: TEXT.register, exact: true })
      .getByText(TEXT.register)
      .click()
    await expect(this.page.getByText(TEXT.loadingAppMessage)).toBeHidden()
  }
}
