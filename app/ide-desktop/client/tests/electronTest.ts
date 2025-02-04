/** @file Commonly used functions for electron tests */

import { test as base, expect } from '@chromatic-com/playwright'
import { _electron, ElectronApplication, type Page } from '@playwright/test'
import { TEXTS } from 'enso-common/src/text'
import os from 'node:os'
import pathModule from 'node:path'

const LOADING_TIMEOUT = 10000
const TEXT = TEXTS.english
export const CONTROL_KEY = os.platform() === 'darwin' ? 'Meta' : 'Control'

/**
 * Tests run on electron executable.
 *
 * Similar to playwright's test, but launches electron, and passes Page of the main window.
 */
export const test = base.extend<{
  projectsDir: string
  app: ElectronApplication
  page: Page
}>({
  // eslint-disable-next-line no-empty-pattern
  projectsDir: async function ({}, use, testInfo) {
    const projectsDir = pathModule.join(os.tmpdir(), 'enso-test-projects', testInfo.testId)
    await use(projectsDir)
  },
  app: async function ({ projectsDir }, use, testInfo) {
    const app = await _electron.launch({
      executablePath: process.env.ENSO_TEST_EXEC_PATH ?? '',
      args: process.env.ENSO_TEST_APP_ARGS != null ? process.env.ENSO_TEST_APP_ARGS.split(',') : [],
      env: { ...process.env, ENSO_TEST: 'true', ENSO_TEST_PROJECTS_DIR: projectsDir },
    })
    await app.context().tracing.start({ screenshots: true, snapshots: true, sources: true })
    await use(app)
    await app.context().tracing.stop({ path: `test-traces/${testInfo.testId}.zip` })
    await app.close()
  },
  page: async function ({ app, viewport }, use) {
    const innerPage = await app.firstWindow()
    if (viewport) innerPage.setViewportSize(viewport)

    const __browserFn = innerPage.context().browser
    Object.defineProperty(innerPage.context(), 'browser', {
      value: function browser() {
        return (
          __browserFn.apply(this) ?? {
            browserType: () => ({
              name: () => 'chromium',
            }),
          }
        )
      },
    })
    await use(innerPage)
  },
})

/**
 * Login as test user. This function asserts that page is the login page, and uses
 * credentials from ENSO_TEST_USER and ENSO_TEST_USER_PASSWORD env variables.
 */
export async function loginAsTestUser(page: Page) {
  // Login screen
  await expect(page.getByText('Login to your account')).toBeVisible({ timeout: LOADING_TIMEOUT })
  await expect(page.getByRole('textbox', { name: 'email' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'password' })).toBeVisible()
  if (process.env.ENSO_TEST_USER == null || process.env.ENSO_TEST_USER_PASSWORD == null) {
    throw Error(
      'Cannot log in; `ENSO_TEST_USER` and `ENSO_TEST_USER_PASSWORD` env variables are not provided',
    )
  }
  await page.getByRole('textbox', { name: 'email' }).fill(process.env.ENSO_TEST_USER)
  await page.getByRole('textbox', { name: 'password' }).fill(process.env.ENSO_TEST_USER_PASSWORD)
  await page.getByTestId('form-submit-button').click()

  await page
    .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
    .getByText(TEXT.licenseAgreementCheckbox)
    .click()
  await page
    .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
    .getByText(TEXT.privacyPolicyCheckbox)
    .click()

  await page.getByTestId('form-submit-button').click()
}
