import { test, type Locator, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate, mockMethodCallInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

/** Prepare the graph for the tests. We add the table type to the `aggregated` node. */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await mockExpressionUpdate(page, 'aggregated', { type: ['Standard.Table.Table.Table'] })
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

test('Copy/paste from Table Visualization', async ({ page, context }) => {
  const expectClipboard = expect.poll(() =>
    page.evaluate(() => window.navigator.clipboard.readText()),
  )
  await context.grantPermissions(['clipboard-read', 'clipboard-write'])
  await actions.goToGraph(page)

  await actions.openVisualization(page, 'Table')
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()
  await tableVisualization.getByText('0,0').hover()
  await page.mouse.down()
  await tableVisualization.getByText('2,1').hover()
  await page.mouse.up()

  // Copy from table visualization
  await page.keyboard.press(`${CONTROL_KEY}+C`)
  await expectClipboard.toMatch(/^0,0\t0,1\r\n1,0\t1,1\r\n2,0\t2,1$/)

  // Paste to Node.
  await actions.clickAtBackground(page)
  const nodesCount = await locate.graphNode(page).count()
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount + 1)
  // Node binding would be `node1` for pasted node.
  const nodeBinding = 'node1'
  await mockMethodCallInfo(page, nodeBinding, {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'input',
    },
    notAppliedArguments: [],
  })
  await expectTableInputContent(page, locate.graphNode(page).last())

  // Paste to Table Widget.
  const node = await actions.createTableNode(page)
  const widget = node.locator('.WidgetTableEditor')
  await expect(widget).toBeVisible()
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await widget.locator('.valueCell').first().click()
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  await expectTableInputContent(page, node)

  // Copy from table input widget
  await node.getByText('0,0').hover()
  await page.mouse.down()
  await node.getByText('2,1').hover()
  await page.mouse.up()
  await page.keyboard.press(`${CONTROL_KEY}+C`)
  await expectClipboard.toMatch(/^0,0\t0,1\r\n1,0\t1,1\r\n2,0\t2,1$/)

  // Copy from table input widget with headers
  await node.getByText('0,0').hover()
  await page.mouse.down()
  await node.getByText('2,1').hover()
  await page.mouse.up()
  await page.mouse.down({ button: 'right' })
  await page.mouse.up({ button: 'right' })
  await page.getByText('Copy with Headers').click()
  await expectClipboard.toMatch(/^Column 1\tColumn 2\r\n0,0\t0,1\r\n1,0\t1,1\r\n2,0\t2,1$/)
})

async function expectTableInputContent(page: Page, node: Locator) {
  const widget = node.locator('.WidgetTableEditor')
  await expect(widget).toBeVisible({ timeout: 5000 })
  await expect(widget.locator('.valueCell')).toHaveText([
    '0,0',
    '0,1',
    '1,0',
    '1,1',
    '2,0',
    '2,1',
    '',
    '',
  ])
}
