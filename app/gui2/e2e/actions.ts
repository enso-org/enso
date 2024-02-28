import { expect, type Page } from '@playwright/test'
import * as customExpect from './customExpect'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

// =================
// === goToGraph ===
// =================

/** Perform a successful login. */
export async function goToGraph(page: Page) {
  await page.goto('/')
  await expect(page.locator('.App')).toBeVisible()
  // Wait until nodes are loaded.
  await customExpect.toExist(locate.graphNode(page))
  // Wait for position initialization
  await expect(locate.graphNode(page).first()).toHaveCSS(
    'transform',
    'matrix(1, 0, 0, 1, -16, -16)',
  )
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
