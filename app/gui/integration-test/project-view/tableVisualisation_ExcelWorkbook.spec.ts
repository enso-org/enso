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

test.only('ExcelWorkbook Visualisation Test', async ({ page }) => {
  
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const tableVisualization = locate.tableVisualization(page)
  await expect(tableVisualization).toExist()

  const col = tableVisualization.getByRole('columnheader', { name: /^0/ })

  await mockVisualizationDataUpdate(
    page,
    'Standard.Visualization.Table.Visualization.prepare_visualization',
    {
      sheet_names: ["Sheet1", "Sheet2", "Sheet3"],
      get_child_node_action: "read",
      type: "Excel_Workbook"
    },
  )
  await expect(tableVisualization).toContainText('Value')
  await expect(tableVisualization).toContainText('Sheet1')
  await expect(tableVisualization).toContainText('Sheet2')
  await expect(tableVisualization).toContainText('Sheet3')
  const sheet2 = tableVisualization.getByText('Sheet2')
  await sheet2.dblclick()
  const newNode = graphNodeByBinding(page, 'node1')
  await expect(newNode).toContainText("read")
  await expect(newNode).toContainText("Sheet2")
})
