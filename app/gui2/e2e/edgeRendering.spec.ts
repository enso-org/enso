import { expect, Page, test } from '@playwright/test'
import * as actions from './actions'
import * as locate from './locate'

async function edgesFromNodeWithBinding(page: Page, binding: string) {
  const node = locate.graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-source-node-id="${nodeId}"]`)
}
async function edgesToNodeWithBinding(page: Page, binding: string) {
  const node = locate.graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-target-node-id="${nodeId}"]`)
}

test('Existence of edges between nodes', async ({ page }) => {
  await actions.goToGraph(page)

  // For each outgoing edge we expect three elements: an element for io, an element for the arrow, and an element for the rendered edge itself.

  await expect(await edgesFromNodeWithBinding(page, 'aggregated')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'filtered')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'data')).toHaveCount(6)
  await expect(await edgesFromNodeWithBinding(page, 'list')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'final')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'prod')).toHaveCount(3)
  await expect(await edgesFromNodeWithBinding(page, 'sum')).toHaveCount(3)
  await expect(await edgesFromNodeWithBinding(page, 'ten')).toHaveCount(3)
  await expect(await edgesFromNodeWithBinding(page, 'five')).toHaveCount(3)

  await expect(await edgesToNodeWithBinding(page, 'aggregated')).toHaveCount(3)
  await expect(await edgesToNodeWithBinding(page, 'filtered')).toHaveCount(3)
  await expect(await edgesToNodeWithBinding(page, 'data')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'list')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'final')).toHaveCount(3)
  await expect(await edgesToNodeWithBinding(page, 'prod')).toHaveCount(3)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(6)
  await expect(await edgesToNodeWithBinding(page, 'ten')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'five')).toHaveCount(0)
})

test('Hover behaviour of edges', async ({ page }) => {
  await actions.goToGraph(page)

  // One for interaction, one for rendering, one for the arrow element.
  await expect(await edgesFromNodeWithBinding(page, 'ten')).toHaveCount(3)

  const targetEdge = page.locator('path:nth-child(4)')
  // Hover over edge to the right of node with binding `ten`.
  await targetEdge.hover({
    position: { x: 65, y: 135.0 },
    force: true,
  })
  // Expect an extra edge for the split rendering.
  const edgeElements = await edgesFromNodeWithBinding(page, 'ten')
  await expect(edgeElements).toHaveCount(4)

  // Expect the top edge part to be dimmed
  const topEdge = page.locator('path:nth-child(4)')
  await expect(topEdge).toHaveClass('edge visible dimmed')
  // Expect the bottom edge part not to be dimmed
  const bottomEdge = page.locator('path:nth-child(6)')
  await expect(bottomEdge).toHaveClass('edge visible')
})
