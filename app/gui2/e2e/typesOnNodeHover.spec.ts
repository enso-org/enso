import { test, type Locator, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

const DUMMY_INT_TYPE = { full: 'Standard.Base.Data.Numbers.Integer', short: 'Integer' }
const DUMMY_STRING_TYPE = { full: 'Standard.Base.Data.Text.Text', short: 'Text' }
const DUMMY_FLOAT_TYPE = { full: 'Standard.Base.Data.Numbers.Float', short: 'Float' }
const UNKNOWN_TYPE = { full: 'Unknown', short: 'Unknown' }
async function assertTypeLabelOnNode(
  page: Page,
  node: Locator,
  type: { full: string; short: string },
) {
  // Ensure the visualization button won't be covered by any other parts of another node (e.g. a comment).
  await bringNodeToFront(page, node)
  await node.hover({ position: { x: 8, y: 8 } })
  await locate.toggleVisualizationButton(node).click()
  const targetLabel = node.locator('.node-type').first()
  await expect(targetLabel).toHaveText(type.short)
  await expect(targetLabel).toHaveAttribute('title', type.full)
  await locate.toggleVisualizationButton(node).click()
  await actions.deselectNodes(page)
}

async function bringNodeToFront(page: Page, node: Locator) {
  await node.click({ position: { x: 8, y: 8 } })
  await page.keyboard.press('Escape')
}

async function assertTypeLabelOnNodeByBinding(
  page: Page,
  label: string,
  type: { full: string; short: string },
) {
  const node = locate.graphNodeByBinding(page, label)
  await assertTypeLabelOnNode(page, node, type)
}

test('shows the correct type when hovering a node', async ({ page }) => {
  await actions.goToGraph(page)

  // Note that the types don't have to make sense, they just have to be applied.
  await mockExpressionUpdate(page, 'five', { type: DUMMY_INT_TYPE.full })
  await mockExpressionUpdate(page, 'ten', { type: DUMMY_STRING_TYPE.full })
  await mockExpressionUpdate(page, 'sum', { type: DUMMY_FLOAT_TYPE.full })
  await mockExpressionUpdate(page, 'prod', { type: DUMMY_INT_TYPE.full })

  await assertTypeLabelOnNodeByBinding(page, 'five', DUMMY_INT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'ten', DUMMY_STRING_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'sum', DUMMY_FLOAT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'prod', DUMMY_INT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'final', UNKNOWN_TYPE)
})
