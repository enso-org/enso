import { expect, type Page } from 'integration-test/base'
import { mockMethodCallInfo } from './expressionUpdates'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

// =============
// === Graph ===
// =============

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
  await page
    .getByTestId('visualization-selector-entries')
    .getByRole('button', { name: visName })
    .click()
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
  await page.keyboard.type('Table.input')
  // Wait for CB entry to appear; this way we're sure about node name (binding).
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveCount(1)
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveText('Table.input')
  await page.keyboard.press('Enter')
  const node = locate.graphNodeByBinding(page, 'any1')
  await expect(node).toHaveCount(1)
  await expect(node).toBeVisible()
  await mockMethodCallInfo(
    page,
    { binding: 'any1', expr: 'Table.input' },
    {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'input',
      },
      notAppliedArguments: [0],
    },
  )
  return node
}
