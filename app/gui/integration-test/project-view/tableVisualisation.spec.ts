import { test, type Locator, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate, mockMethodCallInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'
import { mockVisualizationDataUpdate } from './visualizationUpdates'

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

test('Column size can be set and is retained', async ({ page }) => {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()
  await expect(tableVisualization).toContainText('10 rows.')

  const col = tableVisualization.getByRole('columnheader', { name: /^0/ })
  const colManualSize = await resizeCol(col)

  // A data update causes column autosizing to run
  await mockVisualizationDataUpdate(
    page,
    'Standard.Visualization.Table.Visualization.prepare_visualization',
    {
      type: 'Matrix',
      // eslint-disable-next-line camelcase
      column_count: 5,
      // eslint-disable-next-line camelcase
      all_rows_count: 10,
      json: Array.from({ length: 10 }, (_, i) => Array.from({ length: 5 }, (_, j) => `${i},${j}b`)),
    },
  )
  await expect(tableVisualization).toContainText('0,0b')

  const colSizeAfterDataUpdate = await getElWidth(col)
  expect(colSizeAfterDataUpdate).toBe(colManualSize)
})

async function getElWidth(col: Locator): Promise<number> {
  await col.elementHandle().then((el) => el!.waitForElementState('stable'))
  const bbox = await col.boundingBox()
  expect(bbox).toBeDefined()
  return bbox!.width
}

async function resizeCol(col: Locator): Promise<number> {
  await expect(col).toExist()
  const widthBeforeResize = await getElWidth(col)
  const resizeHandle = col.locator('[data-ref="eResize"]')
  await resizeHandle.dragTo(resizeHandle, {
    sourcePosition: { x: 0, y: 0 },
    targetPosition: { x: 50, y: 0 },
    force: true,
  })
  const widthAfterResize = await getElWidth(col)
  expect(widthAfterResize).toBeGreaterThan(widthBeforeResize)
  return widthAfterResize
}

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

test('Single_Column_Of_Actions Table Visualisation Test', async ({ page }) => {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()

  await mockVisualizationDataUpdate(
    page,
    'Standard.Visualization.Table.Visualization.prepare_visualization',
    /* eslint-disable camelcase */
    {
      type: 'Single_Column_Of_Actions',
      visualization_header: 'table',
      child_label: 'table',
      data: ['Sheet1', 'Sheet2', 'Sheet3'],
      get_child_node_action: 'read',
    },
    /* eslint-enable camelcase */
  )
  await expect(tableVisualization).toContainText('table')
  await expect(tableVisualization).toContainText('Sheet1')
  await expect(tableVisualization).toContainText('Sheet2')
  await expect(tableVisualization).toContainText('Sheet3')
  const sheet2 = tableVisualization.getByText('Sheet2')
  await sheet2.dblclick()
  const newNode = graphNodeByBinding(page, 'node1')
  await expect(newNode).toContainText('read')
  await expect(newNode).toContainText('Sheet2')
})

test('Error Visualisation Test', async ({ page }) => {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()

  await mockVisualizationDataUpdate(
    page,
    'Standard.Visualization.Table.Visualization.prepare_visualization',
    {
      type: 'Error',
      error: 'This is an error message.',
    },
  )
  await expect(tableVisualization).toContainText('This is an error message.')
})
