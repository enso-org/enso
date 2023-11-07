import { test } from '@playwright/test'
import * as actions from './actions'

test('graph can render nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await actions.expectToExist(actions.locateGraphNode(page, (f) => f.visible()))
})
