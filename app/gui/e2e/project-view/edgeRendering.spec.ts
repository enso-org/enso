import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { edgesFromNodeWithBinding, edgesToNodeWithBinding } from './locate'

// For each outgoing edge we expect two elements: an element for io and an element for the rendered edge itself.
const EDGE_PARTS = 2
// For a split edge we expect an extra element for the split rendering.
const SPLIT_EDGE_PARTS = EDGE_PARTS + 1

test('Existence of edges between nodes', async ({ page }) => {
  await actions.goToGraph(page)

  await expect(await edgesFromNodeWithBinding(page, 'aggregated')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'filtered')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'data')).toHaveCount(4 * EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'list')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'final')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'selected')).toHaveCount(0)
  await expect(await edgesFromNodeWithBinding(page, 'prod')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'sum')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'ten')).toHaveCount(EDGE_PARTS)
  await expect(await edgesFromNodeWithBinding(page, 'five')).toHaveCount(EDGE_PARTS)

  await expect(await edgesToNodeWithBinding(page, 'selected')).toHaveCount(EDGE_PARTS)
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

/** Prepare the graph for the tests. We drag the `ten` node to the right for better access to its outgoing edge. */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
  await actions.dragNodeByBinding(page, 'ten', 400, 0)
}

/**
 * Scenario: We hover over the edge to the left of the `ten` node. We expect the edge to be rendered with a dimmed part
 * and a non-dimmed part.
 */
test('Hover behaviour of edges', async ({ page }) => {
  await initGraph(page)

  const edgeElements = await edgesFromNodeWithBinding(page, 'ten')
  await expect(edgeElements).toHaveCount(EDGE_PARTS)

  const targetEdge = edgeElements.and(page.locator('.io'))
  await expect(targetEdge).toExist()

  // Hover over edge to the left of node with binding `ten`.
  await targetEdge.hover({
    position: { x: 250, y: 35.0 },
    force: true,
  })

  // Expect an extra edge for the split rendering.
  const hoveredEdgeElements = await edgesFromNodeWithBinding(page, 'ten')
  await expect(hoveredEdgeElements).toHaveCount(SPLIT_EDGE_PARTS)

  // Expect the top edge part to be dimmed
  const topEdge = page.locator('svg.behindNodes g:nth-child(2) path:nth-child(1)')
  await expect(topEdge).toHaveClass('edge visible dimmed')
  // Expect the bottom edge part not to be dimmed
  const bottomEdge = page.locator('svg.behindNodes g:nth-child(2) path:nth-child(3)')
  await expect(bottomEdge).toHaveClass('edge visible')
})
