import { _electron, expect, test } from '@playwright/test'
import { electronTest, loginAsTestUser } from './electronTest'

electronTest('Create new project', async page => {
  await loginAsTestUser(page)
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode'), {}).toBeVisible({ timeout: 30000 })
})
