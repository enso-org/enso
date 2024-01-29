import { expect, Locator, Page, test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

const DUMMY_INT_TYPE = 'Standard.Base.Data.Numbers.Integer'
const DUMMY_STRING_TYPE = 'Standard.Base.Data.Text.Text'
const DUMMY_FLOAT_TYPE = 'Standard.Base.Data.Numbers.Float'
const UNKNOWN_TYPE = 'Unknown'
async function assertTypeLabelOnNode(page: Page, node: Locator, type: string) {
  const targetLabel = node.locator('.outputPortLabel').first()
  await expect(targetLabel).toHaveText(type)
  await expect(targetLabel).toHaveCSS('opacity', '0')

  const outputPortArea = await node.locator('.outputPortHoverArea').boundingBox()
  assert(outputPortArea, 'The outputPortArea of the node is null')
  const outputPortX = outputPortArea.x + outputPortArea.width / 2.0
  const outputPortY = outputPortArea.y + outputPortArea.height - 2.0
  await page.mouse.move(outputPortX, outputPortY)
  await expect(targetLabel).toBeVisible()
  await expect(targetLabel).toHaveCSS('opacity', '1')
}

async function assertTypeLabelOnNodeByBinding(page: Page, label: string, type: string) {
  const node = locate.graphNodeByBinding(page, label)
  await assertTypeLabelOnNode(page, node, type)
}

test('shows the correct type when hovering a node', async ({ page }) => {
  await actions.goToGraph(page)

  // Note that the types don't have to make sense, they just have to be applied.
  await mockExpressionUpdate(page, 'five', { type: DUMMY_INT_TYPE })
  await mockExpressionUpdate(page, 'ten', { type: DUMMY_STRING_TYPE })
  await mockExpressionUpdate(page, 'sum', { type: DUMMY_FLOAT_TYPE })
  await mockExpressionUpdate(page, 'prod', { type: DUMMY_INT_TYPE })

  await assertTypeLabelOnNodeByBinding(page, 'five', DUMMY_INT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'ten', DUMMY_STRING_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'sum', DUMMY_FLOAT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'prod', DUMMY_INT_TYPE)
  await assertTypeLabelOnNodeByBinding(page, 'final', UNKNOWN_TYPE)
})
