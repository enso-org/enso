import { test, type BrowserContext, type Locator, type Page } from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate, mockMethodCallInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'
import singleColumnDates from './table-vis-json/singleColumnDates.json' with { type: 'json' }
import singleColumnDatetimes from './table-vis-json/singleColumnDatetimes.json' with { type: 'json' }
import singleColumnTimes from './table-vis-json/singleColumnTimes.json' with { type: 'json' }
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

test('Single Column Of Actions Table Visualisation Test', async ({ page }) => {
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
      type: 'Generic_Grid',
      headers: [
        { visualization_header: 'table', child_label: 'table', get_child_node_action: 'read' },
      ],
      data: [['Sheet1', 'Sheet2', 'Sheet3']],
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

test('get_child_node_action temmplate Test as number', async ({ page }) => {
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
      type: 'Generic_Grid',
      headers: [
        {
          visualization_header: 'table',
          child_label: 'table',
          get_child_node_action: 'read {{#table}}',
        },
      ],
      data: [['1', '2', '3']],
    },
    /* eslint-enable camelcase */
  )
  await expect(tableVisualization).toContainText('table')
  await expect(tableVisualization).toContainText('1')
  await expect(tableVisualization).toContainText('2')
  await expect(tableVisualization).toContainText('3')
  const value2 = tableVisualization.getByText('2')
  await value2.dblclick()
  const newNode = graphNodeByBinding(page, 'node1')
  await expect(newNode).toContainText('read')
  const numberWidget = newNode.locator('.WidgetNumber')
  await expect(numberWidget).toBeVisible()
  await expect(numberWidget).toHaveValue('2')
})

test('get_child_node_action temmplate Test as text', async ({ page }) => {
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
      type: 'Generic_Grid',
      headers: [
        {
          visualization_header: 'table',
          child_label: 'table',
          get_child_node_action: 'read {{@table}}',
        },
      ],
      data: [['1', '2', '3']],
    },
    /* eslint-enable camelcase */
  )
  await expect(tableVisualization).toContainText('table')
  await expect(tableVisualization).toContainText('1')
  await expect(tableVisualization).toContainText('2')
  await expect(tableVisualization).toContainText('3')
  const value2 = tableVisualization.getByText('2')
  await value2.dblclick()
  const newNode = graphNodeByBinding(page, 'node1')
  const textWidget = newNode.locator('.WidgetText')
  await expect(textWidget).toBeVisible()
  await expect(textWidget.getByTestId('widget-text-content')).toHaveText('2')
})

test('GenericGrid Table Visualisation Test - single column - no links', async ({ page }) => {
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
      type: 'Generic_Grid',
      headers: [{ visualization_header: 'table' }],
      data: [['Sheet1', 'Sheet2', 'Sheet3']],
    },
    /* eslint-enable camelcase */
  )
  await expect(tableVisualization).toContainText('table')
  await expect(tableVisualization).toContainText('Sheet1')
  await expect(tableVisualization).toContainText('Sheet2')
  await expect(tableVisualization).toContainText('Sheet3')
})

test('GenericGrid Table Visualisation Test - two column - link on second', async ({ page }) => {
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
      type: 'Generic_Grid',
      headers: [
        { visualization_header: 'table' },
        { visualization_header: 'number', get_child_node_action: 'read {{#number}} {{@table}}' },
      ],
      data: [
        ['SheetA', 'SheetB', 'SheetC'],
        ['1', '2', '3'],
      ],
    },
    /* eslint-enable camelcase */
  )
  await expect(tableVisualization).toContainText('table')
  await expect(tableVisualization).toContainText('SheetA')
  await expect(tableVisualization).toContainText('SheetB')
  await expect(tableVisualization).toContainText('SheetC')
  await expect(tableVisualization).toContainText('number')
  await expect(tableVisualization).toContainText('1')
  await expect(tableVisualization).toContainText('2')
  await expect(tableVisualization).toContainText('3')
  const value2 = tableVisualization.getByText('2')
  await value2.dblclick()
  const newNode = graphNodeByBinding(page, 'node1')

  await expect(newNode).toContainText('read')

  const textWidget = newNode.locator('.WidgetText')
  await expect(textWidget).toBeVisible()
  await expect(textWidget.getByTestId('widget-text-content')).toHaveText('SheetB')

  const numberWidget = newNode.locator('.WidgetNumber')
  await expect(numberWidget).toBeVisible()
  await expect(numberWidget).toHaveValue('2')
})

/* 
   These tests pair with the Enso tests found at test/Visualization_Tests/src/Table_Visualisation_Integration_Spec.enso
   Those tests check the json produced by prepare_visualization matches a baseline
   These tests check that json data then renders correctly in an AG Grid in the GUI
   If you change the json API you can regen the reference json by commenting in the line of code in 
   check_equal in Table_Visualisation_Integration_Spec.enso and running those tests
   Then run the js prettier
   Remember to comment the write back out
*/

test('Datetime test - sorting and copying', async ({ page, context }) => {
  await loadData(page, singleColumnDatetimes)
  await expectCellDataToBe(
    page,
    'Value',
    '2025-01-02 12:13:14.123[MET]',
    '2025-01-01 12:13:14.123[MET]',
    '2025-01-03 12:13:14.123[MET]',
  )
  const value = await getHeaderLocator(page, { colHeaderName: 'Value' })
  await value.click() // Sort ascending
  await expectCellDataToBe(
    page,
    'Value',
    '2025-01-01 12:13:14.123[MET]',
    '2025-01-02 12:13:14.123[MET]',
    '2025-01-03 12:13:14.123[MET]',
  )
  await value.click() // Sort descending
  await expectCellDataToBe(
    page,
    'Value',
    '2025-01-03 12:13:14.123[MET]',
    '2025-01-02 12:13:14.123[MET]',
    '2025-01-01 12:13:14.123[MET]',
  )
  await value.click() // remove sort
  await expectCellDataToBe(
    page,
    'Value',
    '2025-01-02 12:13:14.123[MET]',
    '2025-01-01 12:13:14.123[MET]',
    '2025-01-03 12:13:14.123[MET]',
  )
  await expectCopyingColumnClipboardToBe(
    page,
    context,
    'Value',
    0,
    1,
    '2025-01-02 12:13:14.123[MET]\r\n2025-01-01 12:13:14.123[MET]',
  )
})

test('Date test - sorting and copying', async ({ page, context }) => {
  await loadData(page, singleColumnDates)
  await expectCellDataToBe(page, 'Value', '2025-01-02', '2025-01-01', '2025-01-03')
  const value = await getHeaderLocator(page, { colHeaderName: 'Value' })
  await value.click({ position: { x: 10, y: 10 } }) // Sort ascending
  await expectCellDataToBe(page, 'Value', '2025-01-01', '2025-01-02', '2025-01-03')
  await value.click({ position: { x: 10, y: 10 } }) // Sort descending
  await expectCellDataToBe(page, 'Value', '2025-01-03', '2025-01-02', '2025-01-01')
  await value.click({ position: { x: 10, y: 10 } }) // remove sort
  await expectCellDataToBe(page, 'Value', '2025-01-02', '2025-01-01', '2025-01-03')
  await expectCopyingColumnClipboardToBe(page, context, 'Value', 0, 1, '2025-01-02\r\n2025-01-01')
})

test('Time test - sorting and copying', async ({ page, context }) => {
  await loadData(page, singleColumnTimes)
  await expectCellDataToBe(page, 'Value', '12:14:14.123004', '12:13:14.123004', '12:15:14.123004')
  const value = await getHeaderLocator(page, { colHeaderName: 'Value' })
  await value.click({ position: { x: 10, y: 10 } }) // Sort ascending
  await expectCellDataToBe(page, 'Value', '12:13:14.123004', '12:14:14.123004', '12:15:14.123004')
  await value.click({ position: { x: 10, y: 10 } }) // Sort descending
  await expectCellDataToBe(page, 'Value', '12:15:14.123004', '12:14:14.123004', '12:13:14.123004')
  await value.click({ position: { x: 10, y: 10 } }) // remove sort
  await expectCellDataToBe(page, 'Value', '12:14:14.123004', '12:13:14.123004', '12:15:14.123004')
  await expectCopyingColumnClipboardToBe(
    page,
    context,
    'Value',
    0,
    1,
    '12:14:14.123004\r\n12:13:14.123004',
  )
})

async function expectCopyingColumnClipboardToBe(
  page: Page,
  context: BrowserContext,
  columnName: string,
  startRow: number,
  endRow: number,
  expectedClipboardText: string,
) {
  await context.grantPermissions(['clipboard-read', 'clipboard-write'])
  await getCellLocator(page, columnName, startRow).click()
  await page.keyboard.down('Shift')
  await getCellLocator(page, columnName, endRow).click()
  await page.keyboard.up('Shift')
  await page.keyboard.press(`${CONTROL_KEY}+C`)
  const expectClipboard = expect.poll(() =>
    page.evaluate(() => window.navigator.clipboard.readText()),
  )
  await expectClipboard.toBe(expectedClipboardText)
}

async function loadData(page: Page, data: any) {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()

  await mockVisualizationDataUpdate(
    page,
    'Standard.Visualization.Table.Visualization.prepare_visualization',
    data,
  )
}

export type ColumnLocatorOptions = {
  colId?: string
  colHeaderName?: string
}

/**
 * Returns a locator for the header cell
 */
export function getHeaderLocator(page: Page, options: ColumnLocatorOptions) {
  if (options.colHeaderName) {
    return page.getByRole('columnheader', { name: options.colHeaderName })
  }
  return page.getByRole('columnheader').and(page.locator(`[col-id="${options.colId}"]`))
}

/**
 * Returns a locator for the cell based off colId and rowIndex
 */
export function getCellLocator(page: Page, colId: string, rowIndex: number) {
  const locatorString = `[row-index="${rowIndex}"] [col-id="${colId}"]`
  return page.locator(locatorString)
}

// Helper function to check cell values in a column
async function expectCellDataToBe(page: Page, colId: string, ...expectedValues: string[]) {
  for (let i = 0; i < expectedValues.length; i++) {
    expect(await getCellLocator(page, colId, i).textContent()).toBe(expectedValues[i])
  }
}
