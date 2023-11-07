import { expect, test } from '@playwright/test'
import * as actions from './actions'
import * as locate from './locate'

test('component browser shows entries, and creates a new node', async ({ page }) => {
  await actions.goToGraph(page)
  await locate.graphEditor(page).click()
  await locate.graphEditor(page).press('Enter')
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()
  const nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserEntry(page).last().click()
  await expect(locate.componentBrowser(page)).not.toExist()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
})
