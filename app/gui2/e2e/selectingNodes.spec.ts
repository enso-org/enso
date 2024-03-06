import { expect, test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'

test('Selecting nodes by click', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  await customExpect.not.toBeSelected(node1)
  await customExpect.not.toBeSelected(node2)

  await locate.graphNodeIcon(node1).click()
  await customExpect.toBeSelected(node1)
  await customExpect.not.toBeSelected(node2)

  await locate.graphNodeIcon(node2).click()
  await customExpect.not.toBeSelected(node1)
  await customExpect.toBeSelected(node2)

  await page.waitForTimeout(600) // Avoid double clicks
  await locate.graphNodeIcon(node1).click({ modifiers: ['Shift'] })
  await customExpect.toBeSelected(node1)
  await customExpect.toBeSelected(node2)

  await locate.graphNodeIcon(node2).click()
  await customExpect.not.toBeSelected(node1)
  await customExpect.toBeSelected(node2)

  await page.mouse.click(200, 200)
  await customExpect.not.toBeSelected(node1)
  await customExpect.not.toBeSelected(node2)
})

test('Selecting nodes by area drag', async ({ page }) => {
  await actions.goToGraph(page)
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  await customExpect.not.toBeSelected(node1)
  await customExpect.not.toBeSelected(node2)

  const node1BBox = await node1.locator('.selection').boundingBox()
  const node2BBox = await node2.boundingBox()
  assert(node1BBox)
  assert(node2BBox)
  await page.mouse.move(node1BBox.x - 50, node1BBox.y - 50)
  await page.mouse.down()
  await page.mouse.move(node1BBox.x - 49, node1BBox.y - 49)
  await expect(page.locator('.SelectionBrush')).toBeVisible()
  await page.mouse.move(node2BBox.x + node2BBox.width, node2BBox.y + node2BBox.height)
  await customExpect.toBeSelected(node1)
  await customExpect.toBeSelected(node2)
  await page.mouse.up()
  await customExpect.toBeSelected(node1)
  await customExpect.toBeSelected(node2)
})
