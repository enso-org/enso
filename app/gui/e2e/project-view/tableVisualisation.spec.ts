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

test('Copy from Table Visualization', async ({ page, context }) => {
  await context.grantPermissions(['clipboard-read', 'clipboard-write'])
  await actions.goToGraph(page)

  actions.openVisualization(page, 'Table')
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()
  await tableVisualization.getByText('0,0').hover()
  await page.mouse.down()
  await tableVisualization.getByText('2,1').hover()
  await page.mouse.up()
  await page.keyboard.press('Control+C')

  // Paste to Node.
  await actions.clickAtBackground(page)
  const nodesCount = await locate.graphNode(page).count()
  await page.keyboard.press('Control+V')
  await expect(locate.graphNode(page)).toHaveCount(nodesCount + 1)
  await expect(locate.graphNode(page).last().locator('input')).toHaveValue(
    '0,0\t0,11,0\t1,12,0\t2,1',
  )

  // Paste to Table Widget.
  const node = await actions.createTableNode(page)
  const widget = node.locator('.WidgetTableEditor')
  await expect(widget).toBeVisible()
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await widget.locator('.ag-cell', { hasNotText: /0/ }).first().click()
  await page.keyboard.press('Control+V')
  await expect(widget.locator('.ag-cell')).toHaveText([
    '0',
    '0,0',
    '0,1',
    '',
    '1',
    '1,0',
    '1,1',
    '',
    '2',
    '2,0',
    '2,1',
    '',
    '3',
    '',
    '',
    '',
  ])
})
