import { test } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { CONTROL_KEY, DELETE_KEY } from './keyboard'
import * as locate from './locate'

test('Deleting selected node with backspace key', async ({ page }) => {
  await actions.goToGraph(page)

  const nodesCount = await locate.graphNode(page).count()
  const deletedNode = locate.graphNodeByBinding(page, 'final')
  await deletedNode.click()
  await page.keyboard.press('Backspace')
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
})

test('Deleting selected node with delete key', async ({ page }) => {
  await actions.goToGraph(page)

  const nodesCount = await locate.graphNode(page).count()
  const deletedNode = locate.graphNodeByBinding(page, 'final')
  await deletedNode.click()
  await page.keyboard.press('Delete')
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
})

test('Graph can be empty', async ({ page }) => {
  await actions.goToGraph(page)

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

test('Removing connected nodes', async ({ page }) => {
  await actions.goToGraph(page)
  const nodesCount = await locate.graphNode(page).count()
  await page.keyboard.down('Shift')
  await locate.graphNodeByBinding(page, 'five').click()
  await expect(locate.selectedNodes(page)).toHaveCount(1)
  await locate.graphNodeByBinding(page, 'sum').click()
  await expect(locate.selectedNodes(page)).toHaveCount(2)
  await page.keyboard.up('Shift')
  await page.keyboard.press('Delete')
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 2)
})
