import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

/** Prepare the graph for the tests. We add the table type to the `aggregated` node. */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await mockExpressionUpdate(page, 'aggregated', { type: 'Standard.Table.Table.Table' })
}

/**
 Scenario: We open the default visualisation of the `aggregated` node. We expect it to be a table visualisation and to
    contain 10 rows and the values 0,0 to 3,0, which are just some sample values that should be visible in the table
    after opening it.
 */
test('Load Table Visualisation', async ({ page }) => {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()
  await expect(tableVisualization).toContainText('10 rows.')
  await expect(tableVisualization).toContainText('0,0')
  await expect(tableVisualization).toContainText('1,0')
  await expect(tableVisualization).toContainText('2,0')
  await expect(tableVisualization).toContainText('3,0')
})
