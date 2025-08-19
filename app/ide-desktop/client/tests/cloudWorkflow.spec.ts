/** @file A series of tests designed for testing GUI behavior in Cloud. */

import { expect } from 'playwright/test'
import { loginAsTestUser, test } from './electronTest'

// Test designed to see, if removing a member from Enso organisation shows imediately in GUI

test('Remove Member', async ({ page }) => {
  await loginAsTestUser(page)

  // If welcome project is to be opened, wait for it and get back to the main dashboard.
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso' })
  await Promise.race([welcomeProjectTab.waitFor({ state: 'visible' }), page.waitForTimeout(3000)])
  if (await welcomeProjectTab.isVisible()) {
    await page.getByRole('tab', { name: 'Data Catalog' }).click()
  }

  // Navigating into the Members settings tab
  await page.getByLabel('User Settings').click()
  await page.getByRole('button', { name: 'Settings' }).click()

  await expect(page.getByText('Settings for')).toBeVisible()

  // Checking the ability to manage oganization
  try {
    await page.getByRole('button', { name: 'Members', exact: true }).click()
  } catch {
    test.skip(true, 'Not a member of an organization.')
    return
  }

  // Wait until members table has at least one row
  await page.locator('table tbody tr').first().waitFor()

  const rows = page.locator('table tbody tr')
  const count = await rows.count()

  if (count >= 2) {
    const secondRow = rows.nth(1)
    const email = await secondRow.locator('td span').first().innerText()

    // Removing a member
    const removeButton = secondRow.getByRole('button', { name: 'Remove' })
    await removeButton.click()

    // Check if the removal was successful
    await expect(page.getByText(email)).not.toBeVisible()
  } else {
    console.log('Couldn’t find enough members in your organization.')
  }
})

// A test created to see, if duplicating projects in Cloud dashboard works.

test('Cloud Project Duplicate', async ({ page }) => {
  await loginAsTestUser(page)

  // If welcome project is to be opened, wait for it.
  // If none for 3 seconds, we just move on.
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso' })
  await Promise.race([welcomeProjectTab.waitFor({ state: 'visible' }), page.waitForTimeout(3000)])
  if (await welcomeProjectTab.isVisible()) {
    await page.getByRole('tab', { name: 'Data Catalog' }).click()
  }

  // Switching to a private cloud folder
  await expect(page.getByRole('button', { name: 'Cloud', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'Cloud', exact: true }).click()

  // Creating a new project
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  await expect(page.locator('.TableVisualization')).toBeVisible({ timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

  // Returning back to the data catalog
  await expect(page.getByRole('tab', { name: 'Data Catalog' })).toBeVisible()
  await page.getByRole('tab', { name: 'Data Catalog' }).click()

  // Finding all of the 'New pojects'
  const projects = await page
    .getByTestId('drive-view')
    .getByText(/New Project \d+/)
    .all()

  const numbered = await Promise.all(
    projects.map(async (p) => {
      const text = await p.innerText()
      const num = parseInt(text.replace('New Project ', ''), 10)
      return { locator: p, num }
    }),
  )

  // Pick the one with the highest number
  const newest = numbered.reduce((a, b) => (a.num > b.num ? a : b)).locator
  await newest.click({ button: 'right' })

  // Try to duplicate the new project
  await expect(page.getByRole('button', { name: 'Duplicate' })).toBeVisible()
  await page.getByRole('button', { name: 'Duplicate' }).click()

  // Checking if the duplication was successful
  expect(page.getByText('New Project 1 (copy)')).toBeVisible()
})
