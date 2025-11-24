/** @file A series of tests designed for testing 'Getting Started with Enso Analytics'. */

import path from 'path'
import { expect } from 'playwright/test'
import {
  closeWelcome,
  createNewProject,
  fillWidgetText,
  loginAsTestUser,
  openComponentBrowser,
  test,
  visualizeData,
  waitForDownload,
} from './electronTest'

// First excercise in Enso Analytics 101
test('Exercise 1', async ({ page, projectsDir }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // ---------------- Objective 1 ----------------
  await test.step('Objective 1: Let’s read a single sheet in from Excel', async () => {
    await createNewProject(page)

    const addComponent = page.getByLabel('Add Component (Enter)')
    await expect(addComponent).toBeVisible()
    await addComponent.click()

    const dataReadEntry = page.locator('.ComponentEntry', { hasText: /^Data\.read$/ })
    await expect(dataReadEntry).toBeVisible()
    await dataReadEntry.click()

    // Create relative path to file
    const filePath = path.join(projectsDir, 'Samples', 'Data', 'sample_bank_data.xlsx')

    // Waiting for file download
    await waitForDownload(filePath)

    // Filling in file url
    await fillWidgetText(page, 'path‘‘', 'Samples/Data/sample_bank_data.xlsx')

    await Promise.race([
      page.getByLabel('Show visualization (Space)').waitFor({ state: 'visible', timeout: 5000 }),
      page
        .getByText(/file not found/i)
        .waitFor({ state: 'visible', timeout: 5000 })
        .catch(() => null),
    ])

    await visualizeData(page)

    // Choosing the first sheet
    await expect(page.getByText('Sheet1')).toBeVisible()
    await page.getByText('Sheet1').dblclick()
  })

  // ---------------- Objective 2 ----------------
  await test.step('Objective 2: Filter Data to find “exception” records', async () => {
    // Adding set component
    await openComponentBrowser(page, 'readquery‘Sheet1’')
    await page.locator('.ComponentEntry', { hasText: 'set' }).click()

    // Set parameters
    await page.locator('.WidgetSelection.clickable').filter({ hasText: 'value' }).click()
    await page.getByRole('button', { name: '<Simple Expression>', exact: true }).click()

    await page.getByText('input', { exact: true }).click()
    await page.getByRole('button', { name: 'currency_code', exact: true }).click()

    await page.getByText('operation', { exact: true }).click()
    await page.getByRole('button', { name: 'Text', exact: true }).click()

    await page.getByText('operation', { exact: true }).click()
    await page.getByRole('button', { name: 'length', exact: true }).click()

    // Typing in the column name
    await fillWidgetText(page, 'as“”', 'currency_code_length')

    // Adding filter component
    await openComponentBrowser(page, 'set')

    await page.locator('.ComponentEntry', { hasText: 'filter' }).click()
    await page.locator('.WidgetSelection.clickable').filter({ hasText: 'column' }).click()

    // Click with the assurance of component being in vision
    const option = page.getByRole('button', { name: 'currency_code_length', exact: true })
    await option.scrollIntoViewIfNeeded()
    await option.click()

    // Choosing the right filter. Wait until selection loads (which should show an arrow).
    const filter = page.locator('.WidgetSelection:has(.arrow)', { hasText: 'filter' })

    await filter.click()

    // Ensuring visibility
    const notEqualBtn = page.getByRole('button', { name: '..Not_Equal', exact: true })
    await notEqualBtn.waitFor({ state: 'visible', timeout: 10000 })
    await notEqualBtn.click()

    await page.locator('.WidgetSelection.clickable').filter({ hasText: /^to$/ }).click()
    await page.getByRole('button', { name: '<Number Value>' }).click()

    // Set the actual filtered number value
    const numberBox = page.locator('input.WidgetNumber')
    await expect(numberBox).toBeVisible()
    await numberBox.fill('3')

    // Visualize data frame
    await visualizeData(page)

    // Checking the total count equals to 1
    await expect(page.getByText('Total Row Count: 1')).toBeVisible()

    // Delete the filter component, to make it easier for objective 3
    await page.getByText('filter', { exact: true }).first().click({ button: 'right' })
    await page.keyboard.press('Delete')
  })

  // ---------------- Objective 3 ----------------
  await test.step('Objective 3: Filter Data to find “Savings Account” records', async () => {
    // Creating filter component
    await page.getByText('set', { exact: true }).click({ button: 'right' })
    await page.keyboard.press('Enter')

    await page.locator('.ComponentEntry', { hasText: 'filter' }).click()
    await page.locator('.WidgetSelection.clickable').filter({ hasText: 'column' }).click()

    // Click with the assurance of component being in vision
    const option2 = page.getByRole('button', { name: 'product_name', exact: true })
    await option2.scrollIntoViewIfNeeded()
    await expect(option2).toBeVisible()
    await option2.click()

    // Choosing the right parameters
    const filter2 = page.locator('.WidgetSelection:has(.arrow)', { hasText: 'filter' })
    await filter2.click()

    await page.getByRole('button', { name: '..Equal', exact: true }).click()

    await page.locator('.WidgetSelection.clickable').filter({ hasText: /^to$/ }).click()
    await page.getByRole('button', { name: '<Text Value>' }).click()

    // Set the filtered text value
    await fillWidgetText(page, 'filter..Equal“”', 'Savings Account')
  })

  // ---------------- Objective 4 ----------------
  // Hardly testable
})
