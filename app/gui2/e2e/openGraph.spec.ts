import { test } from '@playwright/test'
import * as actions from './actions'

test('can open graph', async ({ page }) => {
  await actions.goToGraph(page)
  await actions.expectToExist(actions.locateGraphEditor(page, (f) => f.visible()))
})
