import { expect, test } from '@playwright/test'
import * as actions from './actions'
import { edgesToNodeWithBinding, graphNodeByBinding } from './locate'

test('Disconnect an edge from a port', async ({ page }) => {
  await actions.goToGraph(page)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(6)

  const targetEdge = page.locator('path:nth-child(4)')
  // Hover over edge to the right of node with binding `ten`.
  await targetEdge.click({
    position: { x: 65, y: 135.0 },
    force: true,
  })
  await page.mouse.click(300, 300)
  await page.keyboard.press('Delete')
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
})

test('Connect an node to a port via dragging the edge', async ({ page }) => {
  await actions.goToGraph(page)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(6)

  const targetEdge = page.locator('path:nth-child(4)')
  // Hover over edge to the right of node with binding `ten`.
  await targetEdge.click({
    position: { x: 65, y: 135.0 },
    force: true,
  })
  const targetPort = page.locator('span').filter({ hasText: /^sum$/ })
  await targetPort.click({ force: true, noWaitAfter: true })

  await expect(graphNodeByBinding(page, 'prod')).toContainText('ten')
})
