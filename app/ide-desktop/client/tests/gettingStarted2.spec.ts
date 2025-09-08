/** @file A series of tests designed for testing 'Getting Started with Enso Analytics'. */

import { expect } from 'playwright/test'
import {
  closeWelcome,
  createComponentText,
  createNewProject,
  fillText,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

// First excercise in Enso Analytics 101
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
    await fillText(page, 'path‘‘', 'Samples/Data/sample_bank_data.xlsx')

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
    await createComponentText(page, 'readquery‘Sheet1’')
    await page.locator('.ComponentEntry', { hasText: 'aggregate' }).click()

    // Choosing parameters
    const groupBy = page.getByText('group_by', { exact: true })

    // Ensuring 'plus' is visible, to avoid clicking too early
    await expect(page.locator('div').getByLabel('Add a new item').last()).toBeVisible()
    await groupBy.click()

    const productBtn = page.getByRole('button', { name: 'product_name', exact: true })

    // If dropdown menu doesn't open, click groupBy again to avoid test flakyness
    if (!(await productBtn.isVisible())) {
      await groupBy.click()
    }
    await productBtn.isVisible()
    await productBtn.click()

    // Close the dropdown
    await page.getByText('aggregate').click()

    // Click the plus
    await page
      .locator('div.WidgetTopLevelArgument', { hasText: 'columns' })
      .getByRole('list')
      .filter({ hasText: /^$/ })
      .getByLabel('Add a new item')
      .click()

    // Visualize and assert the result
    await visualizeData(page)
    await expect(page.getByText('1026')).toBeVisible()

    // Adding sort component
    await createComponentText(page, 'aggregate')
    await page.locator('.ComponentEntry', { hasText: 'sort' }).click()

    // Click the plus
    await page
      .locator('div.WidgetTopLevelArgument', { hasText: 'columns' })
      .nth(1)
      .getByRole('list')
      .filter({ hasText: /^$/ })
      .getByLabel('Add a new item')
      .click()

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

    await page.locator('.WidgetSelection.clickable').filter({ hasText: 'names' }).click()
    const curRCode = page.getByRole('button', { name: 'currency_code' }).first()
    await expect(curRCode).toBeVisible()
    await curRCode.click()

    await page.getByText('values', { exact: true }).click()
    await page.getByRole('button', { name: '..Count_Distinct', exact: true }).click()

    // Click the plus and select argument
    const plus = page
      .locator('div.WidgetTopLevelArgument', { hasText: 'columns' })
      .getByRole('list')
      .filter({ hasText: /^$/ })
      .getByLabel('Add a new item')
    await expect(plus).toBeVisible()
    await plus.click()
    await page.getByRole('button', { name: 'account_id', exact: true }).click()

    await visualizeData(page)

    // Move cross_tab a bit down for more clearance
    const crossTab = page.getByText('cross_tab')

    await crossTab.hover()
    await page.mouse.down()
    const box = await crossTab.boundingBox()
    if (box) {
      await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2 + 200)
    }
    await page.mouse.up()
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
    await page.getByText('value', { exact: true }).click()
    await page.getByRole('button', { name: '<Simple Expression>', exact: true }).click()

    await page.getByText('input', { exact: true }).click()
    await page.getByRole('button', { name: 'currency_code', exact: true }).click()

    await page.getByText('operation', { exact: true }).click()
    await page.getByRole('button', { name: 'if', exact: true }).click()

    await page.getByText('condition', { exact: true }).click()
    await page.getByRole('button', { name: '..Equal', exact: true }).click()

    await page.locator('.WidgetSelection.clickable').filter({ hasText: /^to$/ }).click()
    await page.getByRole('button', { name: '<Text Value>' }).click()

    // Write in the textbox
    await fillText(page, '..Equal“”', 'G')

    await page.getByText('true_value', { exact: true }).click()
    await page.getByRole('button', { name: '<Text Value>', exact: true }).click()

    // // Write in the textbox
    await fillText(page, '..If(..Equal“G”)“”', 'GBP')

    await page.getByText('false_value', { exact: true }).click()
    const option = page.getByRole('button', { name: 'currency_code', exact: true })
    await option.hover()
    await option.click()

    // Write in the textbox
    await fillText(page, 'as“”', 'currency_code')
    await visualizeData(page)
    await expect(page.getByText('191')).toBeVisible()
  })
})
