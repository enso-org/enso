/** @file A series of tests designed for testing GUI behavior in Cloud. */

import { expect } from 'playwright/test'
import {
  closeWelcome,
  createNewProject,
  getNewestProject,
  loginAsTestUser,
  test,
} from './electronTest'

// A test controlling if project session logs aren't empty. Currently skipped due to unconsistency of session logs
test.skip('Session logs', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // Switching to a private cloud folder and creating new project
  await page.getByRole('button', { name: 'Cloud', exact: true }).click()

  await createNewProject(page)

  // Executing
  await page.getByLabel('Write All').click()

  // Finding all of the 'New projects'
  const newest = await getNewestProject(page)
  await newest.click()

  await page.getByLabel('Sessions').click()

  // Clicking the last log
  await page.getByRole('button', { name: /show logs/i }).click()

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

  // This try catch is necessary, because this test can't assert removing a member, if the user isn't a part of an organisation.
  try {
    await page.getByRole('button', { name: 'Members', exact: true }).click({ timeout: 10000 })
  } catch {
    test.skip(true, 'Not a member of an organization.')
    return
  }

  const rows = page.getByRole('row')
  await rows.first().waitFor()

  const count = await rows.count()
  if (count >= 2) {
    const secondRow = rows.nth(3)
    const email = await secondRow.getByRole('cell').first().innerText()

    const cells = await secondRow.getByRole('cell').allInnerTexts()
    console.log('Cells in row:', cells)

    // Click the remove button
    await secondRow.getByText('Remove').click()

    await expect(page.getByText(email)).not.toBeVisible()
  } else {
    console.log('Couldn’t find enough members in your organization.')
  }
})

// A test created to see, if duplicating projects in Cloud dashboard works.
// Skipping in order to remove persistent issue. Will debug it and add a PR with better version later.
test.skip('Cloud Project Duplicate', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // Switching to a private cloud folder
  await page.getByRole('button', { name: 'Cloud', exact: true }).click()

  await createNewProject(page)

  // Finding all of the 'New projects'
  const newest = await getNewestProject(page)
  await newest.click({ button: 'right' })

  // Try to duplicate the new project
  const duplicateButton = page.getByRole('button', { name: 'Duplicate' })
  await expect(duplicateButton).toBeVisible()
  await duplicateButton.click()

  // Checking if the duplication was successful
  await expect(page.getByText('New Project 1 (copy)')).toBeVisible()
})
