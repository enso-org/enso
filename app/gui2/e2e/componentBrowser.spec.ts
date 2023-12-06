import { expect, test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'

test('component browser shows entries, and creates a new node', async ({ page }) => {
  await actions.goToGraph(page)
  await locate.graphEditor(page).click()
  await locate.graphEditor(page).press('Enter')
  await customExpect.toExist(locate.componentBrowser(page))
  await customExpect.toExist(locate.componentBrowserEntry(page))
  let nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserEntry(page).last().click()
  await expect(locate.componentBrowser(page)).not.toBeVisible()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)

  // Clicking at highlighted entry should also work.
  nodeCount = await locate.graphNode(page).count()
  await locate.graphEditor(page).press('Enter')
  await locate.componentBrowserSelectedEntry(page).first().click()
  await expect(locate.componentBrowser(page)).not.toBeVisible()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
})
