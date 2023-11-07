import { expect, test } from '@playwright/test'
import * as actions from './actions'
import * as locate from './locate'

test('graph can open and render nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await expect(locate.graphEditor(page, (f) => f.visible())).toExist()
  await expect(locate.graphNode(page, (f) => f.visible())).toExist()
})
