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
  const nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserEntry(page).last().click()
  await locate.componentBrowserEntry(page).last().click({ force: true })
  await expect(locate.componentBrowser(page)).not.toBeVisible()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
})
