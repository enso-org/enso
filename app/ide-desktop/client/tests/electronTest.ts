/** @file Commonly used functions for electron tests */

import { _electron, expect, type Page, test } from '@playwright/test'

/**
 * Tests run on electron executable.
 *
 * Similar to playwright's test, but launches electron, and passes Page of the main window.
 */
export function electronTest(name: string, body: (page: Page) => Promise<void> | void) {
  test(name, async () => {
    const app = await _electron.launch({
      executablePath: process.env.ENSO_TEST_EXEC_PATH ?? '',
      args: process.env.ENSO_TEST_APP_ARGS != null ? process.env.ENSO_TEST_APP_ARGS.split(',') : [],
      env: { ...process.env, ['ENSO_TEST']: name },
    })
    const page = await app.firstWindow()
    await body(page)
    await app.close()
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
  await page.keyboard.insertText(process.env.ENSO_TEST_USER)
  await page.keyboard.press('Tab')
  await page.keyboard.insertText(process.env.ENSO_TEST_USER_PASSWORD)
  await page.keyboard.press('Enter')

  // Accept terms screen
  await expect(page.getByText('I agree')).toHaveCount(2)
  await expect(page.getByRole('button')).toHaveCount(1)
  for (const checkbox of await page.getByText('I agree').all()) {
    await checkbox.click()
  }
  await page.getByRole('button').click()
}
