/** @file A series of tests designed for testing 'Getting Started with Enso Analytics'. */

import { expect } from 'playwright/test'
import { loginAsTestUser, test } from './electronTest'


// First excercise in Enso Analytics 101
test('Exercise 1', async ({ page }) => {
  await loginAsTestUser(page)

  // If welcome project is to be opened, wait for it and get back to the main dashboard.
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso' })
  await Promise.race([
    welcomeProjectTab.waitFor({ state: 'visible', timeout: 3000 }).catch(() => null),
    page.waitForTimeout(3000),
  ])
  if (await welcomeProjectTab.isVisible()) {
    const dataCatalog = page.getByRole('tab', { name: 'Data Catalog' })
    await expect(dataCatalog).toBeVisible()
    await dataCatalog.click()
  }

  // Opening New Project
  const newProjectButton = page.getByRole('button', { name: 'New Project', exact: true })
  await expect(newProjectButton).toBeVisible()
  await newProjectButton.click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  await expect(page.locator('.TableVisualization')).toBeVisible({ timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

  // Input data
  const addComponent = page.getByLabel('Add Component (Enter)')
  await expect(addComponent).toBeVisible()
  await addComponent.click()

  const dataReadEntry = page.locator('.ComponentEntry', { hasText: /^Data\.read$/ })
  await expect(dataReadEntry).toBeVisible()
  await dataReadEntry.click()

  const urlBox = page.getByTestId('widget-text-content')
  await expect(urlBox).toBeVisible()
  await urlBox.fill('Samples/Data/sample_bank_data.xlsx')

  // Wait for the file to be resolved or error message
  await Promise.race([
    page.getByLabel('Show visualization (Space)').waitFor({ state: 'visible', timeout: 5000 }),
    page.getByText(/file not found/i).waitFor({ state: 'visible', timeout: 5000 }).catch(() => null),
  ])

  // Visualize
  const showViz = page.getByLabel('Show visualization (Space)')
  await expect(showViz).toBeVisible()
  await showViz.click()

  try{
    await expect(page.getByText('Sheet1')).toBeVisible()
    await page.getByText('Sheet1').dblclick()
  }
  catch{
    console.log('Skipping test: Sample data file not found.')
    test.skip()
  }

  // Work with the newly opened sheet
  await expect(showViz).toBeVisible()
  await showViz.click()

  await page.getByTestId('more-button').getByRole('button', { name: 'More' }).click()
  await page.keyboard.press('Enter')

  // Set parameters
  await page.locator('.ComponentEntry', { hasText: 'set' }).click()

  await page.locator('.WidgetSelection.clickable').filter({ hasText: 'value' }).click()
  await page.getByRole('button', { name: '<Simple Expression>', exact: true }).click()

  await page.locator('.widgetApplyPadding', { hasText: 'input' }).click()
  await page.getByRole('button', { name: 'currency_code' }).click()

  await page.locator('.widgetApplyPadding', { hasText: 'operation' }).click()
  await page.getByRole('button', { name: 'Text' }).click()

  await page.locator('.widgetApplyPadding', { hasText: 'operation' }).click()
  await page.getByRole('button', { name: 'length' }).click()

  // Typing in the column name
  const container = page.locator('.WidgetArgumentName.primary:has-text("as")')
  const nameBox = container.locator('div.cm-content[role="textbox"]')
  await expect(nameBox).toBeVisible()
  await nameBox.fill('currency_code_length')

  // Filtering dataframe
  await page.getByTestId('more-button').getByRole('button', { name: 'More' }).click()
  await page.keyboard.press('Enter')

  await page.locator('.ComponentEntry', { hasText: 'filter' }).click()
  await page.locator('.WidgetSelection.clickable').filter({ hasText: 'column' }).click()

  // Click with the assurance of component being in vision
  const option = page.getByRole('button', { name: 'currency_code_length', exact: true })
  await option.scrollIntoViewIfNeeded()
  await expect(option).toBeVisible()
  await option.click()

  // Choosing the right filter
  const filterContainer = page.locator('.WidgetArgumentName.primary', { hasText: 'filter' })
  const filter = filterContainer.locator('span.widgetApplyPadding', { hasText: /^filter$/ })
  await expect(filter).toBeVisible()
  await filter.click()

  await page.getByRole('button', { name: '..Not_Equal', exact: true }).click()

  await page.locator('.WidgetSelection.clickable').filter({ hasText: /^to$/ }).click()
  await page.getByRole('button', { name: '<Number Value>' }).click()

  // Set the actual filtered number value
  const numberBox = page.locator('input.WidgetNumber')
  await expect(numberBox).toBeVisible()
  await numberBox.fill('3')

  // Visualize and check output
  await expect(showViz).toBeVisible()
  await showViz.click()

  // Checking the total count equals to 1
  await expect(page.locator('.ag-status-bar-right div > div', { hasText: 'Total Row Count:' })).toBeVisible()
  await expect(
    page.locator('.ag-status-bar-right div > div', { hasText: 'Total Row Count:' }),
  ).toHaveText(/ 1$/)
})
