/** @file A series of tests designed for testing 'Getting Started with Enso Analytics'. */

import { expect } from 'playwright/test'
import { createNewComponent, loginAsTestUser, test, visualizeData } from './electronTest'

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

  // ---------------- Objective 1 ----------------
  await test.step('Objective 1: Let’s read a single sheet in from Excel', async () => {
    const newProjectButton = page.getByRole('button', { name: 'New Project', exact: true })
    await expect(newProjectButton).toBeVisible()
    await newProjectButton.click()
    await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

    await expect(page.locator('.TableVisualization')).toBeVisible({ timeout: 30000 })
    await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

    const addComponent = page.getByLabel('Add Component (Enter)')
    await expect(addComponent).toBeVisible()
    await addComponent.click()

    const dataReadEntry = page.locator('.ComponentEntry', { hasText: /^Data\.read$/ })
    await expect(dataReadEntry).toBeVisible()
    await dataReadEntry.click()

    const urlBox = page.getByTestId('widget-text-content')
    await expect(urlBox).toBeVisible()
    await urlBox.fill('Samples/Data/sample_bank_data.xlsx')

    await Promise.race([
      page.getByLabel('Show visualization (Space)').waitFor({ state: 'visible', timeout: 5000 }),
      page
        .getByText(/file not found/i)
        .waitFor({ state: 'visible', timeout: 5000 })
        .catch(() => null),
    ])

    visualizeData(page)

    try {
      await expect(page.getByText('Sheet1')).toBeVisible()
      await page.getByText('Sheet1').dblclick()
    } catch {
      console.log('Skipping test: Sample data file not found.')
      test.skip()
    }

    visualizeData(page)
  })

  // ---------------- Objective 2 ----------------
  await test.step('Objective 1: Filter Data to find “exception” records', async () => {
    // Adding set component
    createNewComponent(page)
    await page.locator('.ComponentEntry', { hasText: 'set' }).click()

    // Set parameters
    await page.locator('.WidgetSelection.clickable').filter({ hasText: 'value' }).click()
    await page.getByRole('button', { name: '<Simple Expression>', exact: true }).click()

    await page.locator('.widgetApplyPadding', { hasText: 'input' }).click()
    await page.getByRole('button', { name: 'currency_code', exact: true }).click()

    await page.locator('.widgetApplyPadding', { hasText: 'operation' }).click()
    await page.getByRole('button', { name: 'Text', exact: true }).click()

    await page.locator('.widgetApplyPadding', { hasText: 'operation' }).click()
    await page.getByRole('button', { name: 'length', exact: true }).click()

    // Typing in the column name
    const container = page.locator('.WidgetArgumentName.primary:has-text("as")')
    const nameBox = container.locator('div.cm-content[role="textbox"]')
    await expect(nameBox).toBeVisible()
    await nameBox.fill('currency_code_length')

    // Adding filter component
    createNewComponent(page)
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

    // Visualize data frame
    visualizeData(page)

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
    const filterContainer2 = page.locator('.WidgetArgumentName.primary', { hasText: 'filter' })
    const filter2 = filterContainer2.locator('span.widgetApplyPadding', { hasText: /^filter$/ })
    await expect(filter2).toBeVisible()
    await filter2.click()

    await page.getByRole('button', { name: '..Equal', exact: true }).click()

    await page.locator('.WidgetSelection.clickable').filter({ hasText: /^to$/ }).click()
    await page.getByRole('button', { name: '<Text Value>' }).click()

    // Set the filtered text value
    const filterInput = page.getByTestId('widget-text-content').last()
    await expect(filterInput).toBeVisible()
    await filterInput.fill('Savings Account')

    // Visualize data frame
    visualizeData(page)
  })

  // ---------------- Objective 4 ----------------
  await test.step('Objective 4: Using the Zoom controls to show more or less of the workflow', async () => {
    const seeLess = page.getByLabel('Increase Zoom')
    const showAll = page.getByLabel('Show All Components (Ctrl + Shift + A)')

    // Seeing how many components are visible
    await showAll.click()
    const initialCount = await page.locator('div.content').count()

    // Zoom in and assert if the component count is diferent
    for (let i = 0; i < 5; i++) {
      await seeLess.click()
    }
    const zoomedCount = await page.locator('div.content').count()
    await expect(zoomedCount).not.toBe(initialCount)
  })
})
