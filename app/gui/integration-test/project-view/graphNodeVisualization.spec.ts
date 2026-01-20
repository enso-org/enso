import assert from 'assert'
import { expect, test } from 'integration-test/base'
import * as actions from './actions'
import { computedContent } from './css'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

test('Node can open and load visualization', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'final')
  await node.click({ position: { x: 8, y: 8 } })
  await expect(locate.componentMenu(page)).toExist()
  await locate.toggleVisualizationButton(page).click()
  await expect(locate.anyVisualization(page)).toExist()
  await expect(locate.loadingVisualization(page)).toHaveCount(0)
  await locate.toggleVisualizationSelectorButton(page).click()
  await page.getByText('JSON').click()
  const vis = locate.jsonVisualization(page)
  await expect(vis).toExist()
  // The default JSON viz data contains an object.
  const element = await vis.elementHandle()
  assert(element != null)
  const textContent = await computedContent(element)
  const jsonContent = JSON.parse(textContent)
  expect(typeof jsonContent).toBe('object')
  const nodeType = await locate.visualisationNodeType(page)
  await expect(nodeType).toHaveText('Unknown')
  await mockExpressionUpdate(page, 'final', { type: ['Standard.Table.Table.Table'] })
  await expect(nodeType).toHaveText('Table')
  await mockExpressionUpdate(page, 'final', { type: ['Standard.Table.Table.DifferentType'] })
  await expect(nodeType).toHaveText('DifferentType')
})

// FIXME: Previewing visualization behavior currently seems inconsistent
test.skip('Previewing visualization', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNode(page).last()
  const port = await locate.outputPortCoordinates(page, node)
  await editorPage.down('Mod')
  await expect(locate.anyVisualization(page)).toBeHidden()
  await page.mouse.move(port.x, port.y)
  await expect(locate.anyVisualization(node)).toBeVisible()
  await editorPage.up('Mod')
  await page.mouse.move(port.x + 1, port.y)
  await expect(locate.anyVisualization(page)).toBeHidden()
  await editorPage.down('Mod')
  await page.mouse.move(port.x, port.y)
  await expect(locate.anyVisualization(node)).toBeVisible()
  // TODO[ao]: The simple move near top-left corner not always works i.e. not always
  //  `pointerleave` event is emitted. Investigated in https://github.com/enso-org/enso/issues/9478
  await page.mouse.move(500, 1200, { steps: 20 })
  await expect(locate.anyVisualization(page)).toBeHidden()
  await editorPage.up('Mod')
  await page.mouse.move(port.x + 1, port.y)
  await expect(locate.anyVisualization(page)).toBeHidden()
})

test('Warnings visualization', async ({ editorPage, page }) => {
  await editorPage
  // Without centering the graph, menu sometimes goes out of the view.
  await page.keyboard.press(`ControlOrMeta+Shift+A`)
  // Create a node, attach a warning, open the warnings-visualization.
  await locate.addNewNodeButton(page).click()

  await locate.componentBrowserInput(page).fill('Warning.attach "Uh oh" 42')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await actions.openVisualization(page, 'Warnings')
  await expect(locate.warningsVisualization(page)).toExist()
  // Click the remove-warnings button, and ensure a node is created.
  const nodeCount = await locate.graphNode(page).count()
  await page.getByTestId('remove-warnings-button').click()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
})
