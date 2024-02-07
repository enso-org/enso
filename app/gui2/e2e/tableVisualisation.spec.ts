import { expect, Page, test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'
import { edgesToNodeWithBinding, graphNodeByBinding } from './locate'

/**
 * Prepare the graph for the tests. We add the table type to  to the `aggregated` node.
 */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await mockExpressionUpdate(page, 'aggregated', { type: 'Standard.Table.Data.Table.Table' })
}

// For each outgoing edge we expect two elements: an element for io and an element for the rendered edge itself.
const EDGE_PARTS = 2

/**
 Scenario: We open the default visualisation of the `aggregated` node. We expect it to be a table visualisation and to
    contain 10 rows and the values 0,0 to 3,0, which are just some sample values that should be visible in the table
    after opening it.
 */
test('Load Table Visualisation', async ({ page }) => {
  await initGraph(page)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2 * EDGE_PARTS)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await customExpect.toExist(tableVisualization)
  await expect(tableVisualization).toContainText('10 rows.')
  await expect(tableVisualization).toContainText('0,0')
  await expect(tableVisualization).toContainText('1,0')
  await expect(tableVisualization).toContainText('2,0')
  await expect(tableVisualization).toContainText('3,0')
})
