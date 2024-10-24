/** @file A test for basic flow of the application: open project and see if nodes appear. */

import { expect } from '@playwright/test'
import { electronTest, loginAsTestUser } from './electronTest'

electronTest('Create new project', async page => {
  await loginAsTestUser(page)
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  // We see the node type and visualization, so the engine is running the program
  await expect(page.locator('.node-type')).toHaveText('Table')
  await expect(page.locator('.TableVisualization')).toBeVisible()
  await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

  // We can add new node and see suggestions.
  await page.locator('.GraphNode').click()
  await page.keyboard.press('Enter')
  await expect(page.locator('.ComponentBrowser')).toBeVisible()
  const entry = page.locator('.ComponentList .list-variant.selected .component', {
    hasText: 'column_count',
  })
  await expect(entry).toBeVisible()
  await entry.click()
  await expect(page.locator('.GraphNode'), {}).toHaveCount(2)
  await page.locator('.GraphNode', { hasText: 'column_count' }).click()
  await page
    .locator('.GraphNode', { hasText: 'column_count' })
    .getByRole('button', { name: 'Visualization' })
    .click()
})
