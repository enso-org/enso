/** @file Constants for integration tests. */
import { TEXTS } from 'enso-common/src/text'
import test, { type Page } from 'playwright/test'

/** An example password that does not meet validation requirements. */
export const INVALID_PASSWORD = 'password'
/** An example password that meets validation requirements. */
export const VALID_PASSWORD = 'Password0!'
/** An example valid email address. */
export const VALID_EMAIL = 'email@example.com'
export const TEXT = TEXTS.english

/** Pass the Agreements dialog. */
export async function passAgreementsDialog(page: Page) {
  await test.step('Accept Terms and Conditions', async () => {
    await page.waitForSelector('#agreements-modal')
    await page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox)
      .click()
    await page
      .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
      .getByText(TEXT.privacyPolicyCheckbox)
      .click()
    await page.getByRole('button', { name: TEXT.accept }).click()
  })
}
