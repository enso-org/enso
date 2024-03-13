import { type Page } from '@playwright/test'
import { expect } from './customExpect'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

// =================
// === goToGraph ===
// =================

/** Perform a successful login. */
export async function goToGraph(page: Page) {
  await page.goto('/')
  // Initial load through vite can take a while. Make sure that the first locator has enough time.
  await expect(page.locator('.GraphEditor')).toBeVisible({ timeout: 100000 })
  // Wait until nodes are loaded.
  await expect(locate.graphNode(page)).toExist()
  // Wait for position initialization
  await expectNodePositionsInitialized(page, 64)
}

export async function expectNodePositionsInitialized(page: Page, yPos: number) {
  // TODO: The yPos should not need to be a variable. Instead, first automatically positioned nodes
  // should always have constant known position. This is a bug caused by incorrect layout after
  // entering a function. To be fixed with #9255
  await expect(locate.graphNode(page).first()).toHaveCSS(
    'transform',
    `matrix(1, 0, 0, 1, -16, ${yPos})`,
  )
}

export async function exitFunction(page: Page, x = 300, y = 300) {
  await page.mouse.dblclick(x, y, { delay: 10 })
}

// =================
// === Drag Node ===
// =================

/// Move node defined by the given binding  by the given x and y.
export async function dragNodeByBinding(page: Page, nodeBinding: string, x: number, y: number) {
  const node = graphNodeByBinding(page, nodeBinding)
  const grabHandle = await node.locator('.grab-handle')
  await grabHandle.dragTo(grabHandle, {
    targetPosition: { x, y },
    force: true,
  })
}
