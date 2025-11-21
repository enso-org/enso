import { expect, test } from 'integration-test/base'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

test('graph can open and render nodes', async ({ editorPage, page }) => {
  await editorPage
  await expect(locate.graphEditor(page)).toExist()
  await expect(locate.graphNode(page)).toExist()

  // check simple node's content (without input widgets)
  const sumNode = locate.graphNodeByBinding(page, 'sum')
  await expect(sumNode.locator('.WidgetToken')).toHaveText(['five', '+', 'ten', '+', 'twenty'])

  // check documented node's content
  const finalNode = locate.graphNodeByBinding(page, 'final')
  await expect(finalNode.locator('.WidgetToken')).toHaveText(['func1'])
})

test('Component icon indicates evaluation in progress', async ({ editorPage, page }) => {
  await editorPage

  const node = locate.graphNodeByBinding(page, 'final')
  await expect(node.locator('.WidgetIcon .LoadingSpinner')).toBeHidden()
  await mockExpressionUpdate(page, 'final', { payload: { type: 'Pending', progress: 0.1 } })
  await expect(node.locator('.WidgetIcon .LoadingSpinner')).toBeVisible()
})

test('Menu is shown when component is hovered', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'final')
  await node.hover({ position: { x: 100, y: 8 } })
  await expect(locate.componentMenu(page)).toExist()
})
