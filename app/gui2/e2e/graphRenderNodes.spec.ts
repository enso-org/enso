import { test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'

test('graph can open and render nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await customExpect.toExist(locate.graphEditor(page))
  await customExpect.toExist(locate.graphNode(page))
})
