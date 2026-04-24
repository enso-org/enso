/** @file Available actions for the login page. */
import { expect, test, type Page } from 'integration-test/base'
import BaseActions, { type LocatorCallback } from './BaseActions'
import DrivePageActions from './DrivePageActions'
import ForgotPasswordPageActions from './ForgotPasswordPageActions'
import RegisterPageActions from './RegisterPageActions'
import { TEXT, VALID_EMAIL, VALID_PASSWORD } from './utilities'

/** Wait for the page to load. */
export async function waitForLoaded(page: Page) {
  await page.waitForLoadState()

  await expect(page.getByTestId(/^(before|after)-auth-layout$/)).toBeAttached({ timeout: 30_000 })
  await expect(page.getByTestId('loading-screen')).toHaveCount(0, { timeout: 30_000 })
}

/** Available actions for the login page. */
export default class LoginPageActions<Context = object> extends BaseActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage() {
    return {
      register: (): RegisterPageActions<Context> =>
        this.step("Go to 'register' page", async (page) => {
          await page.getByRole('link', { name: TEXT.dontHaveAnAccount, exact: true }).click()
          await expect(page.getByRole('button', { name: TEXT.register })).toBeVisible()
        }).into(RegisterPageActions<Context>),
      forgotPassword: (): ForgotPasswordPageActions<Context> =>
        this.step("Go to 'forgot password' page", async (page) => {
          await page.getByRole('link', { name: TEXT.forgotYourPassword, exact: true }).click()
          await expect(page.getByRole('button', { name: TEXT.sendLink })).toBeVisible()
        }).into(ForgotPasswordPageActions<Context>),
    }
  }

  /** Perform a login, but only if not already logged in. */
  loginIfNeeded(email = VALID_EMAIL, password = VALID_PASSWORD) {
    return this.step('Login if needed', async (page) => {
      await waitForLoaded(page)
      const isLoggedIn = (await page.getByTestId('before-auth-layout').count()) === 0
      if (isLoggedIn) {
        test.info().annotations.push({
          type: 'skip',
          description: 'Already logged in',
        })
      } else {
        await this.loginInternal(email, password)
      }
      await expect(page.getByTestId('content-not-allowed')).toHaveCount(0, { timeout: 10_000 })
      const agreementModalVisible = (await page.locator('#agreements-modal').count()) > 0
      if (agreementModalVisible) {
        await this.passAgreementsDialog()
      }
    }).into(DrivePageActions<Context>)
  }

  /** Perform a successful login. */
  login(email = VALID_EMAIL, password = VALID_PASSWORD) {
    return this.step('Login', async () => {
      await this.loginInternal(email, password)
      await this.passAgreementsDialog()
    }).into(DrivePageActions<Context>)
  }

  /** Perform a login as a new user (a user that does not yet have a username). */
  loginAsNewUser(email = VALID_EMAIL, password = VALID_PASSWORD) {
    return this.step('Login (as new user)', async () => {
      await this.loginInternal(email, password)
      await this.passAgreementsDialog()
    }).into(DrivePageActions<Context>)
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
    const next = this.step('Login (should fail)', () => this.loginInternal(email, password, false))
      .expectInputError('email-input', 'email', emailError)
      .expectInputError('password-input', 'password', passwordError)
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
    return this.step('Interact with email input', (page, context) =>
      callback(page.getByPlaceholder(TEXT.emailPlaceholder), context),
    )
  }

  /** Internal login logic shared between all public methods. */
  private async loginInternal(email: string, password: string, expectPass = true) {
    await this.page.getByPlaceholder(TEXT.emailPlaceholder).fill(email)
    await this.page.getByPlaceholder(TEXT.passwordPlaceholder).fill(password)
    await this.page
      .getByRole('button', { name: TEXT.login, exact: true })
      .getByText(TEXT.login)
      .click()
    if (expectPass) {
      await expect(this.page.getByText(TEXT.loginToYourAccount)).toBeHidden()
      await expect(this.page.getByText(TEXT.loadingAppMessage)).toBeHidden()
    }
  }

  private passAgreementsDialog() {
    return test.step('Accept Terms and Conditions', async () => {
      await this.page.waitForSelector('#agreements-modal')
      await this.page
        .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
        .getByText(TEXT.licenseAgreementCheckbox)
        .click()
      await this.page
        .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
        .getByText(TEXT.privacyPolicyCheckbox)
        .click()
      await this.page.getByRole('button', { name: TEXT.accept }).click()
    })
  }
}
