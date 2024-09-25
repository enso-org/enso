import { _electron, expect, Page, test } from '@playwright/test'

export function electronTest(name: string, body: (page: Page) => void | Promise<void>) {
  test(name, async () => {
    const app = await _electron.launch({
      executablePath: '../../../dist/ide/linux-unpacked/enso',
      env: { ...process.env, ['ENSO_TEST']: 'Create new project' },
    })
    const page = await app.firstWindow()
    await body(page)
    await app.close()
  })
}

export async function loginAsTestUser(page: Page) {
  // Login screen
  await expect(page.getByRole('textbox', { name: 'email' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'password' })).toBeVisible()
  await page.keyboard.insertText(process.env.ENSO_TEST_USER ?? '')
  await page.keyboard.press('Tab')
  await page.keyboard.insertText(process.env.ENSO_TEST_USER_PASSWORD ?? '')
  await page.keyboard.press('Enter')

  // Accept terms screen
  await expect(page.getByText('I agree')).toHaveCount(2)
  await expect(page.getByRole('button')).toHaveCount(1)
  for (const checkbox of await page.getByText('I agree').all()) {
    await checkbox.click()
  }
  await page.getByRole('button').click()
}
