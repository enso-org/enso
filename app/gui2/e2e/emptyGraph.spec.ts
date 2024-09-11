import { test } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { CONTROL_KEY, DELETE_KEY } from './keyboard'
import * as locate from './locate'

test('graph can be empty', async ({ page }) => {
  await actions.goToGraph(page)
  await expect(locate.graphEditor(page)).toExist()
  await expect(locate.graphNode(page)).toExist()

  await locate.graphEditor(page).press(`${CONTROL_KEY}+A`)
  await locate.graphEditor(page).press(`${DELETE_KEY}`)

  await expect(locate.graphNode(page)).toHaveCount(0)

  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserInput(page)).toBeVisible()
  await page.keyboard.insertText('foo')
  await page.keyboard.press(`${CONTROL_KEY}+Enter`)
  await expect(locate.graphNode(page)).toHaveCount(1)
  await expect(locate.graphNode(page).locator('.WidgetToken')).toHaveText(['foo'])
})
