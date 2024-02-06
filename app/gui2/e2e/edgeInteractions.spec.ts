import { expect, Page, test } from '@playwright/test'
import * as actions from './actions'
import { edgesToNodeWithBinding, graphNodeByBinding } from './locate'

async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await actions.dragNodeByBinding(page, 'ten', 400, 0)
  await actions.dragNodeByBinding(page, 'sum', -400, 0)
}

const EDGE_PARTS = 2

/**
  Scenario: We disconnect the `sum` parameter in the `prod` node by clicking on the edge and pressing the delete key.
 */
test('Disconnect an edge from a port', async ({ page }) => {
  await initGraph(page)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2 * EDGE_PARTS)

  const targetEdge = page.locator('path:nth-child(4)')

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
  const targetEdge = page.locator('path:nth-child(4)')
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
