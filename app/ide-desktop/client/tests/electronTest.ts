/** @file Commonly used functions for electron tests */

import { _electron, ElectronApplication, expect, type Page, test } from '@playwright/test'
import { TEXTS } from 'enso-common/src/text'
import * as random from 'lib0/random'
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
export function electronTest(
  name: string,
  body: (args: {
    page: Page
    app: ElectronApplication
    projectsDir: string
  }) => Promise<void> | void,
) {
  test(name, async () => {
    const uuid = random.uuidv4()
    const projectsDir = pathModule.join(os.tmpdir(), 'enso-test-projects', `${name}-${uuid}`)
    console.log('Running Application; projects dir is', projectsDir)
    const app = await _electron.launch({
      executablePath: process.env.ENSO_TEST_EXEC_PATH ?? '',
      args: process.env.ENSO_TEST_APP_ARGS != null ? process.env.ENSO_TEST_APP_ARGS.split(',') : [],
      env: { ...process.env, ENSO_TEST: name, ENSO_TEST_PROJECTS_DIR: projectsDir },
    })
    const page = await app.firstWindow()
    await app.context().tracing.start({ screenshots: true, snapshots: true, sources: true })
    // Wait until page will be finally loaded: we expect login screen.
    // There's bigger timeout, because the page may load longer on CI machines.
    await expect(page.getByText('Login to your account')).toBeVisible({ timeout: LOADING_TIMEOUT })
    try {
      await body({ page, app, projectsDir })
    } finally {
      await app.context().tracing.stop({ path: `test-traces/${name}.zip` })
      await app.close()
    }
  })
}

/**
 * Login as test user. This function asserts that page is the login page, and uses
 * credentials from ENSO_TEST_USER and ENSO_TEST_USER_PASSWORD env variables.
 */
export async function loginAsTestUser(page: Page) {
  // Login screen
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
