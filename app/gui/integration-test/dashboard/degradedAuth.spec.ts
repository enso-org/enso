/** @file Verify the degraded-auth mode triggered when `users/me` fails for a non-auth reason. */
import { expect, test } from 'integration-test/base'

import { TEXT, VALID_EMAIL, VALID_PASSWORD } from '../actions'

const HTTP_INTERNAL_SERVER_ERROR = 500
const HTTP_UNAUTHORIZED = 401

// Sign in with fresh storage state so every test exercises the post-login flow.
test.use({ storageState: { cookies: [], origins: [] } })

test('cloud 500 lands in degraded mode and switching to local works', async ({
  loginPage,
  cloudApi,
}) => {
  cloudApi.setUsersMeFailureStatus(HTTP_INTERNAL_SERVER_ERROR)

  await loginPage
    .login(undefined, undefined, { expectAgreements: false })
    .do(async (page) => {
      await expect(page.getByTestId('cloud-unavailable-stub')).toBeVisible({ timeout: 15_000 })
      // While the Cloud is unavailable there is nothing for the agreements to gate, so the
      // prompt is skipped rather than blocking the (still working) local projects.
      await expect(page.locator('#agreements-modal')).toHaveCount(0)
      await expect(page.getByRole('button', { name: TEXT.retry, exact: true })).toBeVisible()
      await expect(
        page.getByRole('button', { name: TEXT.switchToLocal, exact: true }),
      ).toBeVisible()
      // On this branch the category sidebar renders only inside the regular drive layout, so
      // the stub state has no (disabled) Cloud entry to assert on; switching to local goes
      // through the stub's own button.
      await page.getByRole('button', { name: TEXT.switchToLocal, exact: true }).click()
    })
    .withDriveView(async (driveView) => {
      await expect(driveView).toBeVisible()
    })
})

test('retry exits degraded mode once the backend recovers', async ({ loginPage, cloudApi }) => {
  cloudApi.setUsersMeFailureStatus(HTTP_INTERNAL_SERVER_ERROR)

  await loginPage.login(undefined, undefined, { expectAgreements: false }).do(async (page) => {
    await expect(page.getByTestId('cloud-unavailable-stub')).toBeVisible({ timeout: 15_000 })
    cloudApi.setUsersMeFailureStatus(null)
    await page.getByRole('button', { name: TEXT.retry, exact: true }).click()
    await expect(page.getByTestId('cloud-unavailable-stub')).toBeHidden({ timeout: 15_000 })
    // Recovery re-engages the agreements gate that was skipped while the Cloud was unavailable.
    await expect(page.locator('#agreements-modal')).toBeVisible()
    await page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox)
      .click()
    await page
      .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
      .getByText(TEXT.privacyPolicyCheckbox)
      .click()
    await page.getByRole('button', { name: TEXT.accept }).click()
    await expect(page.getByTestId('drive-view')).toBeVisible()
  })
})

test('401 keeps the existing recovery + logout path, no degraded UI', async ({
  loginPage,
  cloudApi,
}) => {
  // Submit credentials by hand: `loginPage.login()` asserts the login form disappears,
  // but the 401-driven unauthorized-recovery flow logs the user back out almost
  // immediately, so the form may never stay hidden long enough.
  cloudApi.setUsersMeFailureStatus(HTTP_UNAUTHORIZED)

  await loginPage.do(async (page) => {
    await page.getByPlaceholder(TEXT.emailPlaceholder).fill(VALID_EMAIL)
    await page.getByPlaceholder(TEXT.passwordPlaceholder).fill(VALID_PASSWORD)
    await page.getByRole('button', { name: TEXT.login, exact: true }).getByText(TEXT.login).click()
    // After unauthorized recovery exhausts, the user is logged out and the login
    // screen is shown again. The cloud-unavailable stub must never appear.
    await expect(page.getByRole('button', { name: TEXT.login, exact: true })).toBeVisible({
      timeout: 30_000,
    })
    await expect(page.getByTestId('cloud-unavailable-stub')).toHaveCount(0)
  })
})
