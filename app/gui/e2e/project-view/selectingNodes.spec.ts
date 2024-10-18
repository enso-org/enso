import { test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { expect } from './customExpect'
import * as locate from './locate'

test('Selecting nodes by click', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'final')
  const selectionMenu = page.locator('.SelectionMenu')
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()
  await expect(selectionMenu).toBeHidden()

  await locate.graphNodeIcon(node1).click()
  await expect(node1).toBeSelected()
  await expect(node2).not.toBeSelected()
  await expect(selectionMenu).toBeHidden()

  // Check that clicking an unselected node deselects replaces the previous selection.
  await locate.graphNodeIcon(node2).click()
  await expect(node1).not.toBeSelected()
  await expect(node2).toBeSelected()
  await expect(selectionMenu).toBeHidden()

  await page.waitForTimeout(300) // Avoid double clicks
  await locate.graphNodeIcon(node1).click({ modifiers: ['Shift'] })
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
  await expect(selectionMenu).toBeVisible()

  // Check that when two nodes are selected, clicking a selected node replaces the previous selection.
  await locate.graphNodeIcon(node2).click()
  await expect(node1).not.toBeSelected()
  await expect(node2).toBeSelected()
  await expect(selectionMenu).toBeHidden()

  // Check that clicking the background deselects all nodes.
  await locate.graphEditor(page).click({ position: { x: 600, y: 200 } })
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()
  await expect(selectionMenu).toBeHidden()
})

test('Selecting nodes by area drag', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()

  const node1Id = await node1.getAttribute('data-node-id')
  const node1Selection = page.locator(`.GraphNodeSelection[data-node-id="${node1Id}"]`)
  const node1BBox = await node1Selection.boundingBox()
  const node2BBox = await node2.boundingBox()
  assert(node1BBox)
  assert(node2BBox)
  await page.mouse.move(node1BBox.x - 50, node1BBox.y - 50)
  await page.mouse.down()
  await page.mouse.move(node1BBox.x - 40, node1BBox.y - 40)
  // await expect(page.locator('.SelectionBrush')).toBeVisible()
  await page.mouse.move(node2BBox.x + node2BBox.width, node2BBox.y + node2BBox.height)
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
  await page.mouse.up()
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
})

test('Moving selected nodes', async ({ page }) => {
  await actions.goToGraph(page)
  const movedNode = locate.graphNodeByBinding(page, 'final')
  const notMovedNode = locate.graphNodeByBinding(page, 'sum')
  await locate.graphNodeIcon(movedNode).click()
  // Selection may affect bounding box: wait until it's actually selected.
  await expect(movedNode).toBeSelected()
  const initialBBox = await movedNode.boundingBox()
  const initialNotMovedBBox = await notMovedNode.boundingBox()
  assert(initialBBox)
  assert(initialNotMovedBBox)
  await page.keyboard.press('ArrowLeft', { delay: 500 })
  const bbox = await movedNode.boundingBox()
  const notMovedBBox = await notMovedNode.boundingBox()
  assert(bbox)
  assert(notMovedBBox)
  await expect(bbox.x).not.toBeCloseTo(initialBBox.x)
  await expect(bbox.y).toBeCloseTo(initialBBox.y)
  await expect(notMovedBBox.x).toBeCloseTo(initialNotMovedBBox.x)
  await expect(notMovedBBox.y).toBeCloseTo(initialNotMovedBBox.y)
})
