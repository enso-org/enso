import test from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { CONTROL_KEY, DELETE_KEY } from './keyboard'
import * as locate from './locate'

test('Adding new node', async ({ page }) => {
  await actions.goToGraph(page)

  const nodesCount = await locate.graphNode(page).count()
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserInput(page)).toBeVisible()
  await page.keyboard.insertText('foo')
  await page.keyboard.press(`${CONTROL_KEY}+Enter`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText(['foo'])
  await expect(locate.rightDock(page)).toBeHidden()
  await expect(page.locator('[data-transitioning]')).toHaveCount(0)
  const newNodeBBox = await locate.graphNode(page).last().boundingBox()

  await page.keyboard.press(`${CONTROL_KEY}+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount)
  await expect(
    locate.graphNode(page).locator('.WidgetToken').filter({ hasText: 'foo' }),
  ).toHaveCount(0)

  await page.keyboard.press(`${CONTROL_KEY}+Shift+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText(['foo'])
  const restoredBox = await locate.graphNode(page).last().boundingBox()
  expect(restoredBox).toEqual(newNodeBBox)
})

test('Removing node', async ({ page }) => {
  await actions.goToGraph(page)

  const nodesCount = await locate.graphNode(page).count()
  const deletedNode = locate.graphNodeByBinding(page, 'final')
  const deletedNodeBBox = await deletedNode.boundingBox()
  await deletedNode.click()
  await page.keyboard.press(DELETE_KEY)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)

  await page.keyboard.press(`${CONTROL_KEY}+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount)
  await expect(deletedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'func1', 'prod'])
  await expect(deletedNode.locator('.GraphNodeComment')).toHaveText('This node can be entered')
  const restoredBBox = await deletedNode.boundingBox()
  expect(restoredBBox).toEqual(deletedNodeBBox)

  await page.keyboard.press(`${CONTROL_KEY}+Shift+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
  await expect(deletedNode).not.toBeVisible()
})
