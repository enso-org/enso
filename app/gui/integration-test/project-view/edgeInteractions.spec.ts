import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'
import { edgesToNodeWithBinding, graphNodeByBinding, outputPortCoordinates } from './locate'

/**
 * Prepare the graph for the tests. We drag the `ten` node to the right of the `sum` node for better access
 * to the edges.
 */
async function initGraph(editorPage: EditorPageActions) {
  await editorPage.dragNode('ten', { x: 400, y: 0 }).dragNode('sum', { x: -400, y: 0 })
}

test('Selecting and deleting edge', async ({ editorPage, page }) => {
  await editorPage

  await initGraph(editorPage)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
  const targetEdge = await locate.connectedEdgesFromNodeWithBinding(page, 'ten')
  // Click edge coming from node with binding `ten`.
  await targetEdge.click({
    position: { x: 270, y: 25.0 },
    force: true,
  })
  await expect(targetEdge.locator('.edge.selected')).toExist()
  await page.keyboard.press('Delete')
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2)
})

test('Deleting edge through context menu', async ({ editorPage, page }) => {
  await editorPage
  await initGraph(editorPage)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
  const targetEdge = await locate.connectedEdgesFromNodeWithBinding(page, 'ten')
  // Right click edge from node with binding `ten`.
  await targetEdge.click({
    button: 'right',
    position: { x: 270, y: 25.0 },
    force: true,
  })
  await expect(targetEdge.locator('.edge.selected')).toExist()
  const menu = page.getByTestId('contextMenu')
  await expect(menu).toBeVisible()
  await menu.getByText('Delete Selected Connection').click()
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2)
})

/**
  Scenario: We disconnect the `sum` parameter in the `prod` node by clicking on the edge and clicking on the background.
 */
test('Disconnect an edge from a port', async ({ editorPage, page }) => {
  await initGraph(editorPage)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
  const targetEdge = await locate.connectedEdgesFromNodeWithBinding(page, 'ten')
  // Hover over edge's arrow of node with binding `ten`.
  await targetEdge.click({
    position: { x: 30, y: 150.0 },
    force: true,
  })
  await page.mouse.click(1000, -500)
  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(2)
})

/** Scenario: We replace the `sum` parameter in the `prod` node` with the `ten` node. */
test('Connect an node to a port', async ({ editorPage, page }) => {
  await initGraph(editorPage)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
  const targetEdge = await locate.connectedEdgesFromNodeWithBinding(page, 'ten')
  // Hover over edge's arrow of node with binding `ten`.
  await targetEdge.click({
    position: { x: 30, y: 150.0 },
    force: true,
  })
  // Click the target port in the `prod` node.
  const targetPort = page.locator('.WidgetToken').filter({ hasText: /^sum$/ })
  // We need `force: true` because edge connecting is handled in capture phase and may result
  // in port change, what confuses playwright's actionability checks.
  await targetPort.click({ force: true, noWaitAfter: true })

  await expect(graphNodeByBinding(page, 'prod')).toContainText('ten')
})

/** As above, but by dragging edge instead of clicking source and target separately. */
test('Connect an node to a port via dragging the edge', async ({ editorPage, page }) => {
  await initGraph(editorPage)

  await expect(await edgesToNodeWithBinding(page, 'sum')).toHaveCount(3)
  const targetEdge = await locate.connectedEdgesFromNodeWithBinding(page, 'ten')
  const targetPort = page.locator('.WidgetToken').filter({ hasText: /^sum$/ })
  // Hover over edge's arrow of node with binding `ten`.
  await targetEdge.dragTo(targetPort, {
    sourcePosition: { x: 30, y: 150.0 },
    force: true,
  })
  await expect(graphNodeByBinding(page, 'prod')).toContainText('ten')
})

test('Conditional ports: Disabled', async ({ editorPage, page }) => {
  await editorPage
  const node = graphNodeByBinding(page, 'filtered')
  const conditionalPort = node.locator('.WidgetPort').filter({ hasText: /^filter$/ })

  // Check that the `enabled` CSS class is not set on disabled `WidgetPort`s.
  await expect(node.locator('.WidgetIcon')).toBeVisible()
  await expect(conditionalPort).not.toHaveClass(/enabled/)

  // When a port is disabled, it doesn't react to hovering with a disconnected edge,
  // and any attempt to connect to it should open the CB.
  const outputPort = await outputPortCoordinates(page, graphNodeByBinding(page, 'final'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await conditionalPort.hover()
  await expect(conditionalPort).not.toHaveClass(/isVisualTarget/)
  // We need `force: true` because ComponentBrowser appears in event's capture phase, what
  // confuses playwright's actionability checks.
  await conditionalPort.click({ force: true })
  await expect(node.locator('.WidgetToken')).toHaveText(['filter'])
  await editorPage.expectEdgesFromTo('final', 'filtered')
})

test('Conditional ports: Enabled', async ({ editorPage, page }) => {
  await editorPage
  const node = graphNodeByBinding(page, 'filtered')
  const conditionalPort = node.locator('.WidgetPort').filter({ hasText: /^filter$/ })

  const outputPort = await outputPortCoordinates(page, graphNodeByBinding(page, 'final'))
  await page.mouse.click(outputPort.x, outputPort.y)

  await page.keyboard.down('ControlOrMeta')
  await expect(conditionalPort).toHaveClass(/enabled/)

  await conditionalPort.hover()
  await expect(conditionalPort).toHaveClass(/isVisualTarget/)
  // We need to force port clicks; see comment in 'Connect an node to a port via dragging the edge'
  await conditionalPort.click({ force: true })
  await expect(node.locator('.WidgetToken')).toHaveText(['final'])

  await page.keyboard.up('ControlOrMeta')
})

test('Edge drop prevents further handling of event', async ({ editorPage, page }) => {
  await editorPage

  const outputPort = await locate.outputPortCoordinates(
    page,
    locate.graphNodeByBinding(page, 'five'),
  )
  await page.mouse.click(outputPort.x, outputPort.y - 25)
  await expect(page.getByTestId('mouse-edited-edge')).toBeAttached()
  await page.waitForTimeout(300) // Avoid double clicks
  await page.mouse.click(outputPort.x, outputPort.y - 25)
  await expect(page.getByTestId('mouse-edited-edge')).toBeAttached({ attached: false })
  await expect(locate.componentBrowser(page)).toExist()
})
