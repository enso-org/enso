import { type Page } from '@playwright/test'
import { expect } from './customExpect'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

// =================
// === goToGraph ===
// =================

/** Perform a successful login. */
export async function goToGraph(page: Page, closeDocPanel: boolean = true) {
  await page.goto('/')
  // Initial load through vite can take a while. Make sure that the first locator has enough time.
  await expect(page.locator('.GraphEditor')).toBeVisible({ timeout: 100000 })
  // Wait until nodes are loaded.
  await expect(locate.graphNode(page)).toExist()
  if (closeDocPanel) {
    await expect(page.getByTestId('rightDock')).toExist()
    await page.getByRole('button', { name: 'Documentation Panel' }).click()
    // Wait for the closing animation.
    await expect(page.getByTestId('rightDock')).toBeHidden()
  }
  // Wait for position initialization
  await expectNodePositionsInitialized(page, -16)
}

/**
 * Run assertions for nodes and edges positions being properly initialized.
 *
 * Usually, after opening project or entering a node, we need some ticks for placing both nodes
 * and edges properly on the screen. If test relies on their positions, it must ensure this
 * initialization is done.
 */
export async function expectNodePositionsInitialized(page: Page, yPos: number) {
  // Wait until edges are initialized and displayed correctly.
  await expect(page.getByTestId('broken-edge')).toBeHidden()
  // Wait until node sizes are initialized.
  await expect(locate.graphNode(page).first().locator('.bgFill')).toBeVisible()
  // TODO: The yPos should not need to be a variable. Instead, first automatically positioned nodes
  // should always have constant known position. This is a bug caused by incorrect layout after
  // entering a function. To be fixed with #9255
  await expect(locate.graphNode(page).first()).toHaveCSS(
    'transform',
    `matrix(1, 0, 0, 1, -16, ${yPos})`,
  )
}

/** Exit the currently opened graph (of collapsed function). */
export async function exitFunction(page: Page, x = 300, y = 300) {
  await locate.graphEditor(page).dblclick({ position: { x, y } })
}

// =================
// === Drag Node ===
// =================

/** Move node defined by the given binding  by the given x and y. */
export async function dragNodeByBinding(page: Page, nodeBinding: string, x: number, y: number) {
  const node = graphNodeByBinding(page, nodeBinding)
  const grabHandle = node.locator('.grab-handle')
  await grabHandle.dragTo(grabHandle, {
    targetPosition: { x, y },
    force: true,
  })
}

/** Move mouse away to avoid random hover events and wait for any circular menus to disappear. */
export async function ensureNoCircularMenusVisibleDueToHovering(page: Page) {
  await page.mouse.move(-1000, 0)
  await expect(locate.circularMenu(page)).toBeHidden()
}

/** Ensure no nodes are selected. */
export async function deselectNodes(page: Page) {
  await page.mouse.click(0, 0)
  await expect(locate.selectedNodes(page)).toHaveCount(0)
}
