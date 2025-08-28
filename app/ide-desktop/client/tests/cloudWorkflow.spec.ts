/** @file A series of tests designed for testing GUI behavior in Cloud. */

import { expect } from 'playwright/test'
import { closeWelcome, createNewProject, loginAsTestUser, test } from './electronTest'

// A test controlling if project session logs aren't empty. Currently skipped due to unconsistency of session logs
test.skip('Session logs', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // Switching to a private cloud folder and creating new project
  await page.getByRole('button', { name: 'Cloud', exact: true }).click()

  await createNewProject(page)

  // Executing
  await page.getByLabel('Write All').click()

  // Returning back to the data catalog
  const dataCatalogTab = page.getByRole('tab', { name: 'Data Catalog' })
  await expect(dataCatalogTab).toBeVisible()
  await dataCatalogTab.click()

  // Finding all of the 'New projects'
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
  const newest = numbered.reduce((a, b) => (a.num > b.num ? a : b)).locator
  await newest.click()

  await page.getByLabel('Sessions').click()

  // Navigating into the last log
  try {
    const firstRow = page.locator('div.flex.flex-row.gap-4.rounded-2xl.p-2').first()
    const showLogsButton = firstRow.getByRole('button', { name: /show logs/i })
    await showLogsButton.click()
  } catch {
    console.log('No session logs available')
  }

  await expect(page.getByText('Starting Language Server')).toBeVisible()
})

// Test designed to see, if removing a member from Enso organisation shows imediately in GUI
test('Remove Member', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // Navigating into the Members settings tab
  await page.getByLabel('User Settings').click()
  await page.getByRole('button', { name: 'Settings' }).click()

  await expect(page.getByText('Settings for')).toBeVisible()

  try {
    await page.getByRole('button', { name: 'Members', exact: true }).click()
  } catch {
    test.skip(true, 'Not a member of an organization.')
    return
  }

  const rows = page.locator('table tbody tr')
  await rows.first().waitFor()

  const count = await rows.count()
  if (count >= 2) {
    const secondRow = rows.nth(1)
    const email = await secondRow.locator('td span').first().innerText()

    await secondRow.getByRole('button', { name: 'Remove' }).click()

    await expect(page.getByText(email)).not.toBeVisible()
  } else {
    console.log('Couldn’t find enough members in your organization.')
  }
})

// A test created to see, if duplicating projects in Cloud dashboard works.
test('Cloud Project Duplicate', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // Switching to a private cloud folder
  await page.getByRole('button', { name: 'Cloud', exact: true }).click()

  await createNewProject(page)

  // Returning back to the data catalog
  const dataCatalogTab = page.getByRole('tab', { name: 'Data Catalog' })
  await expect(dataCatalogTab).toBeVisible()
  await dataCatalogTab.click()

  // Finding all of the 'New projects'
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
  const newest = numbered.reduce((a, b) => (a.num > b.num ? a : b)).locator
  await newest.click({ button: 'right' })

  // Try to duplicate the new project
  const duplicateButton = page.getByRole('button', { name: 'Duplicate' })
  await expect(duplicateButton).toBeVisible()
  await duplicateButton.click()

  // Checking if the duplication was successful
  await expect(page.getByText('New Project 1 (copy)')).toBeVisible()
})
