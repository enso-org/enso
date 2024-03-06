import { test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { expect } from './customExpect'
import * as locate from './locate'

test('Selecting nodes by click', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()

  const deselectAll = async () => {
    await page.keyboard.press('Escape')
    await expect(node1).not.toBeSelected()
    await expect(node2).not.toBeSelected()
  }

  await locate.graphNodeIcon(node1).click()
  await expect(node1).toBeSelected()
  await expect(node2).not.toBeSelected()
  await deselectAll()

  await locate.graphNodeIcon(node2).click()
  await expect(node1).not.toBeSelected()
  await expect(node2).toBeSelected()

  await page.waitForTimeout(300) // Avoid double clicks
  await locate.graphNodeIcon(node1).click({ modifiers: ['Shift'] })
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
  await deselectAll()

  await locate.graphNodeIcon(node2).click()
  await expect(node1).not.toBeSelected()
  await expect(node2).toBeSelected()
  await deselectAll()
})

test('Selecting nodes by area drag', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()

  const node1BBox = await node1.locator('.selection').boundingBox()
  const node2BBox = await node2.boundingBox()
  assert(node1BBox)
  assert(node2BBox)
  await page.mouse.move(node1BBox.x - 50, node1BBox.y - 50)
  await page.mouse.down()
  await page.mouse.move(node1BBox.x - 49, node1BBox.y - 49)
  await expect(page.locator('.SelectionBrush')).toBeVisible()
  await page.mouse.move(node2BBox.x + node2BBox.width, node2BBox.y + node2BBox.height)
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
  await page.mouse.up()
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
})
