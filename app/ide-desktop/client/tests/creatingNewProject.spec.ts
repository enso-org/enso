import { _electron, expect, test } from '@playwright/test'

test('Create new project', async () => {
  const app = await _electron.launch({
    executablePath: '../../../dist/ide/linux-unpacked/enso',
  })
  const page = await app.firstWindow()

  // TODO Login?

  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode'), {}).toBeVisible({ timeout: 30000 })

  await app.close()
})
