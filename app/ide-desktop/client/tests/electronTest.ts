/** @file Commonly used functions for electron tests */
/* eslint-disable no-empty-pattern */

import { _electron, test as base, ElectronApplication, expect, type Page } from '@playwright/test'
import { TEXTS } from 'enso-common/src/text'
import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'

const LOADING_TIMEOUT = 10000
const TEXT = TEXTS.english
export const CONTROL_KEY = os.platform() === 'darwin' ? 'Meta' : 'Control'

const electronExecutablePath = await (async () => {
  const POSSIBLE_EXEC_PATHS = [
    '../../../../dist/ide/linux-unpacked/enso',
    '../../../../dist/ide/win-unpacked/Enso.exe',
    '../../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso',
    '../../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
  ].map((p) => path.resolve(import.meta.dirname, p))
  try {
    const promises = POSSIBLE_EXEC_PATHS.map((p) => fs.access(p, fs.constants.X_OK).then(() => p))
    return await Promise.any(promises)
  } catch {
    throw Error('Cannot find Enso package')
  }
})()

/**
 * Tests run on electron executable.
 *
 * Similar to playwright's test, but launches electron, and passes Page of the main window.
 */
export const test = base.extend<{
  testRunId: string
  projectsDir: string
  app: ElectronApplication
  page: Page
}>({
  testRunId: async function ({}, use, testInfo) {
    await use(`${testInfo.testId}-${Date.now()}`)
  },
  projectsDir: async function ({ testRunId }, use) {
    const projectsDir = path.join(os.tmpdir(), 'enso-test-projects', testRunId)
    await use(projectsDir)
  },

  /**
   * Setup for all tests: Create an electron-based app instance.
   */
  app: async function ({ projectsDir, testRunId, viewport }, use) {
    const args = process.env.ENSO_TEST_APP_ARGS?.split(',') ?? []
    if (viewport) args.push(`--window.size=${viewport.width}x${viewport.height}`)
    const app = await _electron.launch({
      executablePath: electronExecutablePath,
      args,
      env: { ...process.env, ENSO_TEST: 'true', ENSO_TEST_PROJECTS_DIR: projectsDir },
    })
    await app.context().tracing.start({ screenshots: true, snapshots: true, sources: true })
    await use(app)
    await app.context().tracing.stop({ path: `test-traces/${testRunId}.zip` })
    await app.close()
  },
  page: async function ({ app, viewport }, use) {
    const innerPage = await app.firstWindow()
    if (viewport) innerPage.setViewportSize(viewport)
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
