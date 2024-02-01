import { expect, test, type Page } from '@playwright/test'
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

// For each outgoing edge we expect two elements: an element for io and an element for the rendered edge itself.
const EDGE_PARTS = 2

test('Existence of edges between nodes', async ({ page }) => {
  await actions.goToGraph(page)

  await expect(await edgesFromNodeWithBinding(page, 'aggregated')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'filtered')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'data')).toHaveCount(2 * EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'list')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'final')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'prod')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'sum')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'ten')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'five')).toHaveCount(EDGE_PARTS)

  await expect(await edgesToNodeWithBinding(page, 'aggregated')).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNodeWithBinding(page, 'filtered')).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNodeWithBinding(page, 'data')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'list')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'final')).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNodeWithBinding(page, 'prod')).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2 * EDGE_PARTS)
  await expect(await edgesToNodeWithBinding(page, 'ten')).toHaveCount(0)
  await expect(await edgesToNodeWithBinding(page, 'five')).toHaveCount(0)
})

test('Hover behaviour of edges', async ({ page }) => {
  await actions.goToGraph(page)

  const edgeElements = await edgesFromNodeWithBinding(page, 'ten')
  await expect(edgeElements).toHaveCount(EDGE_PARTS)

  const targetEdge = edgeElements.first()
  await expect(targetEdge).toHaveClass('edge io')
  // It is not currently possible to interact with edges in the default node layout.
  // See: https://github.com/enso-org/enso/issues/8938
  /*
  // Hover over edge to the right of node with binding `ten`.
  await targetEdge.hover({
    position: { x: 60, y: 45 }, // source node
  })
  // Expect an extra edge for the split rendering.
  const hoveredEdgeElements = await edgesFromNodeWithBinding(page, 'ten')
  await expect(hoveredEdgeElements).toHaveCount(2 * EDGE_PARTS)

  // Expect the top edge part to be dimmed
  const topEdge = page.locator('path:nth-child(4)')
  await expect(topEdge).toHaveClass('edge visible dimmed')
  // Expect the bottom edge part not to be dimmed
  const bottomEdge = page.locator('path:nth-child(6)')
  await expect(bottomEdge).toHaveClass('edge visible')
   */
})
