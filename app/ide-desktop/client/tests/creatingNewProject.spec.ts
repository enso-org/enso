import { _electron, expect, test } from '@playwright/test'

test('Create new project', async () => {
  const app = await _electron.launch({
    executablePath: '../../../dist/ide/linux-unpacked/enso',
    env: { ...process.env, ['ENSO_TEST']: 'Create new project' },
  })
  const page = await app.firstWindow()

  await expect(page.getByRole('textbox', { name: 'email' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'password' })).toBeVisible()
  await page.keyboard.insertText(process.env.ENSO_TEST_USER ?? '')
  await page.keyboard.press('Tab')
  await page.keyboard.insertText(process.env.ENSO_TEST_USER_PASSWORD ?? '')
  await page.keyboard.press('Enter')

  await expect(page.getByText('I agree')).toHaveCount(2)
  await expect(page.getByRole('button')).toHaveCount(1)
  for (const checkbox of await page.getByText('I agree').all()) {
    await checkbox.click()
  }
  await page.getByRole('button').click()

  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode'), {}).toBeVisible({ timeout: 30000 })

  await app.close()
})
