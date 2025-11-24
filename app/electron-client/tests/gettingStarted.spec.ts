/** @file A series of tests designed for testing 'Getting Started with Enso Analytics'. */

import path from 'path'
import { expect } from 'playwright/test'
import {
  addFirstElementToWidgetVector,
  closeWelcome,
  createNewProject,
  fillWidgetText,
  loginAsTestUser,
  openComponentBrowser,
  openDropdownInWidget,
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
    await openDropdownInWidget(page, 'value')
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
    await openDropdownInWidget(page, 'column')

    // Click with the assurance of component being in vision
    const option = page.getByRole('button', { name: 'currency_code_length', exact: true })
    await option.scrollIntoViewIfNeeded()
    await option.click()

    openDropdownInWidget(page, 'filter')
    const notEqualBtn = page.getByRole('button', { name: '..Not_Equal', exact: true })
    await notEqualBtn.waitFor({ state: 'visible', timeout: 10000 })
    await notEqualBtn.click()

    await openDropdownInWidget(page, 'to')
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
    await openDropdownInWidget(page, 'column')

    // Click with the assurance of component being in vision
    const option2 = page.getByRole('button', { name: 'product_name', exact: true })
    await option2.scrollIntoViewIfNeeded()
    await expect(option2).toBeVisible()
    await option2.click()

    // Choosing the right parameters
    await openDropdownInWidget(page, 'filter')
    await page.getByRole('button', { name: '..Equal', exact: true }).click()
    await openDropdownInWidget(page, 'to')
    await page.getByRole('button', { name: '<Text Value>' }).click()

    // Set the filtered text value
    await fillWidgetText(page, 'filter..Equal“”', 'Savings Account')
  })

  // ---------------- Objective 4 ----------------
  // Hardly testable
})

// Second exercise in Enso Analytics 101
test('Exercise 2', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)

  // ---------------- Objective 1 ----------------
  await test.step('Objective 1: Let’s read a single sheet in from Excel', async () => {
    await createNewProject(page)

    // Close documentation
    await page.getByRole('tab', { name: 'Documentation' }).click()

    const addComponent = page.getByLabel('Add Component (Enter)')
    await expect(addComponent).toBeVisible()
    await addComponent.click()

    const dataReadEntry = page.locator('.ComponentEntry', { hasText: /^Data\.read$/ })
    await expect(dataReadEntry).toBeVisible()
    await dataReadEntry.click()

    // Fill in the url
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
    const sheet1 = await page.getByText('Sheet1')
    await expect(sheet1).toBeVisible()
    await sheet1.dblclick()

    await visualizeData(page)
  })

  // ---------------- Objective 2 ----------------
  await test.step('Objective 2:  Aggregating and ranking Account Type', async () => {
    // Decrease zoom
    await page.getByLabel('Decrease Zoom').click()

    // Adding aggregate component
    await openComponentBrowser(page, 'readquery‘Sheet1’')
    await page.locator('.ComponentEntry', { hasText: 'aggregate' }).click()

    // Choosing parameters
    const groupBy = page.getByText('group_by', { exact: true })

    // Ensuring 'plus' is visible, to avoid clicking too early
    await expect(page.locator('div').getByLabel('Add a new item').last()).toBeVisible()
    await groupBy.click()

    const productBtn = page.getByRole('button', { name: 'product_name', exact: true })
    await productBtn.click()

    // Close the dropdown
    await page.getByText('aggregate').click()

    await addFirstElementToWidgetVector(
      page.locator('div.WidgetTopLevelArgument', { hasText: 'columns' }),
    )

    // Visualize and assert the result
    await visualizeData(page)
    await expect(page.getByText('1026')).toBeVisible()

    // Adding sort component
    await openComponentBrowser(page, 'aggregate')
    await page.locator('.ComponentEntry', { hasText: 'sort' }).click()

    await addFirstElementToWidgetVector(
      page.locator('div.WidgetTopLevelArgument', { hasText: 'columns' }),
    )

    // Choosing parameters
    await page
      .locator('div')
      .filter({ hasText: /^‘product_name’$/ })
      .nth(4)
      .click()
    await page.getByRole('button', { name: 'Count', exact: true }).click()

    await page.getByText('direction', { exact: true }).click()
    await page.getByRole('button', { name: '..Descending', exact: true }).click()

    await visualizeData(page)
  })

  // ---------------- Objective 3 ----------------
  await test.step('Objective 3: Create table of currencies by product names', async () => {
    // Scroll into view
    await page.mouse.wheel(0, -200)

    // Creating cross_tab component
    const readComponent = page.getByText('read', { exact: true }).nth(2)
    await readComponent.click({ button: 'right' })

    await page.keyboard.press('Enter')
    await page.locator('.ComponentEntry', { hasText: 'cross_tab' }).click()

    // Choosing the right parameters
    const crossGroup = await page.getByText('group_by', { exact: true }).nth(1)

    // Ensuring 'plus' is visible, to avoid clicking too early
    await expect(page.locator('div').getByLabel('Add a new item').last()).toBeVisible()
    await crossGroup.click()
    await page.getByRole('button', { name: 'product_name', exact: true }).click()

    await openDropdownInWidget(page, 'names')
    const curRCode = page.getByRole('button', { name: 'currency_code' }).first()
    await expect(curRCode).toBeVisible()
    await curRCode.click()

    await page.getByText('values', { exact: true }).click()
    await page.getByRole('button', { name: '..Count_Distinct', exact: true }).click()

    // Click the plus and select argument
    await addFirstElementToWidgetVector(
      page.locator('div.WidgetTopLevelArgument', { hasText: 'columns' }),
    )
    await page.getByRole('button', { name: 'account_id', exact: true }).click()

    await visualizeData(page)

    // Move cross_tab a bit down for more clearance
    const crossTab = page.getByText('cross_tab')
    await crossTab.dragTo(crossTab, { targetPosition: { x: 0, y: 200 }, force: true })
  })

  // ---------------- Objective 4 ----------------
  await test.step('Objective 4:  Fixing Dirty Data', async () => {
    // Scroll into view
    await page.mouse.wheel(0, -200)

    // Creating set component
    const readComponent = page.getByText('read', { exact: true }).nth(2)
    await readComponent.click({ button: 'right' })

    await page.keyboard.press('Enter')
    await page.locator('.ComponentEntry', { hasText: 'set' }).click()

    // Choosing right parameters
    await openDropdownInWidget(page, 'value')
    await page.getByRole('button', { name: '<Simple Expression>', exact: true }).click()

    await openDropdownInWidget(page, 'input')
    await page.getByRole('button', { name: 'currency_code', exact: true }).click()

    await openDropdownInWidget(page, 'operation')
    await page.getByRole('button', { name: 'if', exact: true }).click()

    await openDropdownInWidget(page, 'condition')
    await page.getByRole('button', { name: '..Equal', exact: true }).click()

    await openDropdownInWidget(page, 'to')
    await page.getByRole('button', { name: '<Text Value>' }).click()

    // Write in the textbox
    await fillWidgetText(page, '..Equal“”', 'G')

    await page.getByText('true_value', { exact: true }).click()
    await page.getByRole('button', { name: '<Text Value>', exact: true }).click()

    // // Write in the textbox
    await fillWidgetText(page, '..If(..Equal“G”)“”', 'GBP', 1)

    await page.getByText('false_value', { exact: true }).click()
    const option = page.getByRole('button', { name: 'currency_code', exact: true })
    await option.hover()
    await option.click()

    // Write in the textbox
    await fillWidgetText(page, 'as“”', 'currency_code')
    await visualizeData(page)
    await expect(page.getByText('191')).toBeVisible()
  })
})
