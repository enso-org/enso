import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import { connectedEdgesFromNodeWithBinding } from './locate'

test('Existence of edges between nodes', async ({ editorPage }) => {
  await editorPage
    .expectEdgesFromTo('aggregated', undefined, 1)
    .expectEdgesFromTo('filtered', undefined, 1)
    .expectEdgesFromTo('data', undefined, 5)
    .expectEdgesFromTo('list', undefined, 1)
    .expectEdgesFromTo('final', undefined, 1)
    .expectEdgesFromTo('selected', undefined, 1)
    .expectEdgesFromTo('prod', undefined, 1)
    .expectEdgesFromTo('sum', undefined, 1)
    .expectEdgesFromTo('ten', undefined, 1)
    .expectEdgesFromTo('five', undefined, 1)
    .expectEdgesFromTo('twenty', undefined, 1)
    .expectEdgesFromTo(undefined, 'aggregated', 1)
    .expectEdgesFromTo(undefined, 'filtered', 1)
    .expectEdgesFromTo(undefined, 'data', 0)
    .expectEdgesFromTo(undefined, 'list', 0)
    .expectEdgesFromTo(undefined, 'final', 1)
    .expectEdgesFromTo(undefined, 'selected', 1)
    .expectEdgesFromTo(undefined, 'prod', 1)
    .expectEdgesFromTo(undefined, 'sum', 3)
    .expectEdgesFromTo(undefined, 'ten', 0)
    .expectEdgesFromTo(undefined, 'five', 0)
    .expectEdgesFromTo(undefined, 'twenty', 0)
})

/** Prepare the graph for the tests. We drag the `ten` node to the right for better access to its outgoing edge. */
async function initGraph(editorPage: EditorPageActions) {
  await editorPage.dragNode('ten', { x: 400, y: 0 })
}

/**
 * Scenario: We hover over the edge to the left of the `ten` node. We expect the edge to be rendered with a dimmed part
 * and a non-dimmed part.
 */
test('Hover behaviour of edges', async ({ editorPage, page }) => {
  await initGraph(editorPage)

  const edgeElements = await connectedEdgesFromNodeWithBinding(page, 'ten')
  await expect(edgeElements).toHaveCount(1)

  const targetEdge = edgeElements.locator('.io')
  await expect(targetEdge).toExist()

  // Hover over edge to the left of node with binding `ten`.
  await targetEdge.hover({
    position: { x: 250, y: 35.0 },
    force: true,
  })

  // Expect an extra edge for the split rendering.
  const hoveredEdgeElements = await connectedEdgesFromNodeWithBinding(page, 'ten')
  await expect(hoveredEdgeElements).toHaveCount(1)

  // Expect the top edge part to be dimmed
  const topEdge = page.locator('svg.behindNodes g:nth-child(2) path:nth-child(1)')
  await expect(topEdge).toHaveClass('edge define-node-colors visible dimmed pending')
  // Expect the bottom edge part not to be dimmed
  const bottomEdge = page.locator('svg.behindNodes g:nth-child(2) path:nth-child(3)')
  await expect(bottomEdge).toHaveClass('edge define-node-colors visible pending')
})
