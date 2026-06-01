/** @file Verify the degraded-auth mode triggered when `users/me` fails for a non-auth reason. */
import { expect, test } from 'integration-test/base'

import { TEXT } from '../actions'

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
    .login()
    .do(async (page) => {
      await expect(page.getByTestId('cloud-unavailable-stub')).toBeVisible({ timeout: 15_000 })
      await expect(
        page.getByRole('button', { name: TEXT.retry, exact: true }),
      ).toBeVisible()
      await expect(
        page.getByRole('button', { name: TEXT.switchToLocal, exact: true }),
      ).toBeVisible()
      // The cloud sidebar entry is still rendered but the button is disabled.
      await expect(
        page.getByRole('button', { name: TEXT.cloudCategory }).first(),
      ).toBeDisabled()
    })
    .goToCategory.local()
    .withDriveView(async (driveView) => {
      await expect(driveView).toBeVisible()
    })
})

test('retry exits degraded mode once the backend recovers', async ({ loginPage, cloudApi }) => {
  cloudApi.setUsersMeFailureStatus(HTTP_INTERNAL_SERVER_ERROR)

  await loginPage
    .login()
    .do(async (page) => {
      await expect(page.getByTestId('cloud-unavailable-stub')).toBeVisible({ timeout: 15_000 })
      cloudApi.setUsersMeFailureStatus(null)
      await page.getByRole('button', { name: TEXT.retry, exact: true }).click()
      await expect(page.getByTestId('cloud-unavailable-stub')).toBeHidden({ timeout: 15_000 })
      await expect(page.getByTestId('drive-view')).toBeVisible()
    })
})

test('401 keeps the existing recovery + logout path, no degraded UI', async ({
  loginPage,
  cloudApi,
}) => {
  cloudApi.setUsersMeFailureStatus(HTTP_UNAUTHORIZED)

  await loginPage.login().do(async (page) => {
    // The unauthorized-recovery flow eventually logs the user out, which makes the
    // login form visible again. The degraded `cloud-unavailable` stub must never show.
    await expect(
      page.getByRole('button', { name: TEXT.login, exact: true }),
    ).toBeVisible({ timeout: 30_000 })
    await expect(page.getByTestId('cloud-unavailable-stub')).toHaveCount(0)
  })
})
