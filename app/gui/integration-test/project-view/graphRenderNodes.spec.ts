import { test } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

test('graph can open and render nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await expect(locate.graphEditor(page)).toExist()
  await expect(locate.graphNode(page)).toExist()

  // check simple node's content (without input widgets)
  const sumNode = locate.graphNodeByBinding(page, 'sum')
  await expect(sumNode.locator('.WidgetToken')).toHaveText(['five', '+', 'ten'])

  // check documented node's content
  const finalNode = locate.graphNodeByBinding(page, 'final')
  await expect(finalNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'func1', 'prod'])
})

test('Component icon indicates evaluation in progress', async ({ page }) => {
  await actions.goToGraph(page)

  const node = locate.graphNodeByBinding(page, 'final')
  await expect(node.locator('.WidgetIcon .LoadingSpinner')).not.toBeVisible()
  await mockExpressionUpdate(page, 'final', { payload: { type: 'Pending', progress: 0.1 } })
  await expect(node.locator('.WidgetIcon .LoadingSpinner')).toBeVisible()
})
