import { expect, test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'

test('graph can open and render nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await customExpect.toExist(locate.graphEditor(page))
  await customExpect.toExist(locate.graphNode(page))

  // check simple node's content (without input widgets)
  const sumNode = locate.graphNodeByBinding(page, 'sum')
  await expect(sumNode.locator('.WidgetToken')).toHaveText(['five', '+', 'ten'])

  // check documented node's content
  const finalNode = locate.graphNodeByBinding(page, 'final')
  await expect(finalNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'func1', 'prod'])
})
