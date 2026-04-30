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

/** Prepare the graph for the tests. We drag one of the nodes to the right for better access to its outgoing edge. */
async function initGraph(editorPage: EditorPageActions) {
  await editorPage.dragNode('twenty', { x: 400, y: 0 })
}

/**
 * Scenario: We hover over the arrow of an edge. We expect the edge to be rendered with a dimmed part
 * and a non-dimmed part.
 */
test('Hover behaviour of edges', async ({ editorPage, page }) => {
  await initGraph(editorPage)

  const edgeElements = await connectedEdgesFromNodeWithBinding(page, 'twenty')
  await expect(edgeElements).toHaveCount(1)

  const targetEdge = edgeElements.locator('.io')
  await expect(targetEdge).toExist()

  // Hover near the target end of the edge so `clickWillDisconnect` is true and the
  // edge renders with a `dimmed hovered` lower part plus a non-dimmed upper part
  // (see `activePath` in `GraphEdge.vue`). The bounding box is unsuitable for
  // picking a point — for an L-shaped path it's mostly empty space, and `.io` has
  // `pointer-events: stroke`, so the pointer must actually land on the path.
  // Query the path geometry to get a point 5 units (in path-length) from the end,
  // well within `TARGET_DISCONNECT_THRESHOLD = 10`. Convert SVG-local coordinates
  // to viewport via `getScreenCTM` so `page.mouse.move` lands on the stroke.
  // The two-step move ensures `pointerenter` fires (a single `mousemove` at the
  // target may leave the pointer already "inside" without firing enter).
  const hoverPoint = await targetEdge.evaluate((el) => {
    const path = el as SVGPathElement
    const length = path.getTotalLength()
    const local = path.getPointAtLength(Math.max(0, length - 5))
    const ctm = path.getScreenCTM()
    if (ctm == null) throw new Error('Edge path has no screen CTM')
    return {
      x: ctm.a * local.x + ctm.c * local.y + ctm.e,
      y: ctm.b * local.x + ctm.d * local.y + ctm.f,
    }
  })
  await page.mouse.move(hoverPoint.x - 30, hoverPoint.y)
  await page.mouse.move(hoverPoint.x, hoverPoint.y, { steps: 5 })

  // Expect an extra edge for the split rendering.
  const hoveredEdgeElements = await connectedEdgesFromNodeWithBinding(page, 'twenty')
  await expect(hoveredEdgeElements).toHaveCount(1)

  // Expect the bottom edge part to be dimmed
  const bottomEdge = edgeElements.locator('path.edge').first()
  await expect(bottomEdge).toHaveClass('edge define-node-colors visible dimmed hovered pending')
  // Expect the bottom edge part to be dimmed
  const topEdge = edgeElements.locator('path.edge').last()
  await expect(topEdge).toHaveClass('edge define-node-colors visible pending')
})
