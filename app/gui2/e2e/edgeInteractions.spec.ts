import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import * as locate from './locate'
import { edgesToNodeWithBinding, graphNodeByBinding, outputPortCoordinates } from './locate'

/**
 * Prepare the graph for the tests. We drag the `ten` node to the right of the `sum` node for better access
 * to the edges.
 */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await actions.dragNodeByBinding(page, 'ten', 400, 0)
  await actions.dragNodeByBinding(page, 'sum', -400, 0)
}

// For each outgoing edge we expect two elements: an element for io and an element for the rendered edge itself.
const EDGE_PARTS = 2

/**
  Scenario: We disconnect the `sum` parameter in the `prod` node by clicking on the edge and pressing the delete key.
 */
test('Disconnect an edge from a port', async ({ page }) => {
  await initGraph(page)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2 * EDGE_PARTS)

  const targetEdge = page.locator('svg.behindNodes g:nth-child(2) path.edge.visible')

  // Hover over edge to the right of node with binding `ten`.
  await targetEdge.click({
    position: { x: 250, y: 5.0 },
    force: true,
  })
  await page.mouse.click(500, -500)
  await page.keyboard.press('Delete')
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(EDGE_PARTS)
})

/**
 * Scenario: We replace the `sum` parameter in the `prod` node` with the `ten` node.
 */
test('Connect an node to a port via dragging the edge', async ({ page }) => {
  await initGraph(page)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2 * EDGE_PARTS)
  const targetEdge = page.locator('svg.behindNodes g:nth-child(2) path.edge.visible')
  // Hover over edge to the left of node with binding `ten`.
  await targetEdge.click({
    position: { x: 450, y: 5.0 },
    force: true,
  })
  // Click the target port in the `prod` node.
  const targetPort = page.locator('span').filter({ hasText: /^sum$/ })
  await targetPort.click({ force: true, noWaitAfter: true })

  await expect(graphNodeByBinding(page, 'prod')).toContainText('ten')
})

test('Conditional ports: Disabled', async ({ page }) => {
  await initGraph(page)
  const node = graphNodeByBinding(page, 'filtered')
  const conditionalPort = node.locator('.WidgetPort').filter({ hasText: /^filter$/ })

  // Check that the `enabled` CSS class is not set on disabled `WidgetPort`s.
  await expect(node.locator('.WidgetSelfIcon')).toBeVisible()
  await expect(conditionalPort).not.toHaveClass(/enabled/)

  // When a port is disabled, it doesn't react to hovering with a disconnected edge,
  // and any attempt to connect to it should open the CB.
  const outputPort = await outputPortCoordinates(graphNodeByBinding(page, 'sum'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await conditionalPort.hover()
  await expect(conditionalPort).not.toHaveClass(/isTarget/)
  await conditionalPort.click()
  await expect(locate.componentBrowser(page)).toExist()
  await page.keyboard.press('Escape')
})

test('Conditional ports: Enabled', async ({ page }) => {
  await initGraph(page)
  const node = graphNodeByBinding(page, 'filtered')
  const conditionalPort = node.locator('.WidgetPort').filter({ hasText: /^filter$/ })

  await page.keyboard.down('Meta')
  await page.keyboard.down('Control')

  await expect(conditionalPort).toHaveClass(/enabled/)
  const outputPort = await outputPortCoordinates(graphNodeByBinding(page, 'sum'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await conditionalPort.hover()
  await expect(conditionalPort).toHaveClass(/isTarget/)
  await conditionalPort.click()
  await expect(node.locator('.WidgetToken')).toHaveText(['sum'])

  await page.keyboard.up('Meta')
  await page.keyboard.up('Control')
})
