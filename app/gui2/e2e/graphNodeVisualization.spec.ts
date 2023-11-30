import { expect, test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'

test('node can open and load visualization', async ({ page }) => {
  await actions.goToGraph(page)
  const node = locate.graphNode(page).last()
  await node.click({ position: { x: 8, y: 8 } })
  await customExpect.toExist(locate.circularMenu(page))
  await locate.toggleVisualizationButton(page).click()
  await customExpect.toExist(locate.anyVisualization(page))
  await locate.showVisualizationSelectorButton(page).click()
  await page.getByText('JSON').click()
  await customExpect.toExist(locate.jsonVisualization(page))
  // The default JSON viz data contains an object.
  await expect(locate.jsonVisualization(page)).toContainText('{')
})
