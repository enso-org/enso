/** @file A test for basic flow of the application: open project and see if nodes appear. */

/* eslint-disable @typescript-eslint/no-magic-numbers */

import { expect } from '@playwright/test'
import { electronTest, loginAsTestUser } from './electronTest'

electronTest('Create new project', async page => {
  await loginAsTestUser(page)
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode'), {}).toBeVisible({ timeout: 30000 })
})
