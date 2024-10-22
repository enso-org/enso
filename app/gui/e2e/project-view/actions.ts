import { type Page } from '@playwright/test'
import { expect } from './customExpect'
import { mockMethodCallInfo } from './expressionUpdates'
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

// =============
// === Graph ===
// =============

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

/** Click at spot where no node is. */
export function clickAtBackground(page: Page, x = 300, y = 300) {
  return locate.graphEditor(page).click({ position: { x, y } })
}

// ======================
// === Visualizations ===
// ======================

/**
 *  Open Table visualization
 *
 * This action ensures the table visualization is opened somewhere in the graph; currently it opens
 * visualization for `aggregate` node.
 */
export async function openVisualization(page: Page, visName: string) {
  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await locate.toggleVisualizationSelectorButton(page).click()
  await page.locator('.VisualizationSelector').getByRole('button', { name: visName }).click()
}

// ===============
// === Widgets ===
// ===============

/**
 * Create a Node with Table Input Widget.
 *
 * This function relies on automatically assigned binding and assome no more table nodes exist.
 */
export async function createTableNode(page: Page) {
  // Adding `Table.new` component will display the widget
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await page.keyboard.type('Table.new')
  // Wait for CB entry to appear; this way we're sure about node name (binding).
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveCount(1)
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveText('Table.new')
  await page.keyboard.press('Enter')
  const node = locate.graphNodeByBinding(page, 'table1')
  await expect(node).toHaveCount(1)
  await expect(node).toBeVisible()
  await mockMethodCallInfo(
    page,
    { binding: 'table1', expr: 'Table.new' },
    {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'new',
      },
      notAppliedArguments: [0],
    },
  )
  return node
}
