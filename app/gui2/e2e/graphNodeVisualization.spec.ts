import { test } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import * as locate from './locate'

test('node can open and load visualization', async ({ page }) => {
  await actions.goToGraph(page)
  const node = locate.graphNode(page).last()
  await node.click({ position: { x: 8, y: 8 } })
  await expect(locate.circularMenu(page)).toExist()
  await locate.toggleVisualizationButton(page).click()
  await expect(locate.anyVisualization(page)).toExist()
  await locate.showVisualizationSelectorButton(page).click()
  await page.getByText('JSON').click()
  await expect(locate.jsonVisualization(page)).toExist()
  // The default JSON viz data contains an object.
  await expect(locate.jsonVisualization(page)).toContainText('{')
})
