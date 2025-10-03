import { expect, test, type Locator, type Page } from 'integration-test/base'
import * as actions from './actions'
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
  await node.hover({ position: { x: 8, y: 8 }, force: true })
  await locate.toggleVisualizationButton(node).click({ force: true })
  const targetLabel = node.locator('.node-type').first()
  await expect(targetLabel).toHaveText(type.short)
  await locate.toggleVisualizationButton(node).click({ force: true })
  await actions.deselectNodes(page)
}

async function bringNodeToFront(page: Page, node: Locator) {
  await node.click({ position: { x: 0, y: 8 }, force: true })
  await page.keyboard.press('Escape')
}

function assertTypeLabelOnNodeByBinding(label: string, type: { full: string; short: string }) {
  return async (page: Page) => {
    const node = locate.graphNodeByBinding(page, label)
    await assertTypeLabelOnNode(page, node, type)
  }
}

test('shows the correct type when hovering a node', async ({ editorPage }) => {
  await editorPage
    // Note that the types don't have to make sense, they just have to be applied.
    .mockExpressionUpdate('five', { type: [DUMMY_INT_TYPE.full] })
    .mockExpressionUpdate('ten', { type: [DUMMY_STRING_TYPE.full] })
    .mockExpressionUpdate('sum', { type: [DUMMY_FLOAT_TYPE.full] })
    .mockExpressionUpdate('prod', { type: [DUMMY_INT_TYPE.full] })
    .do(assertTypeLabelOnNodeByBinding('five', DUMMY_INT_TYPE))
    .do(assertTypeLabelOnNodeByBinding('ten', DUMMY_STRING_TYPE))
    .do(assertTypeLabelOnNodeByBinding('sum', DUMMY_FLOAT_TYPE))
    .do(assertTypeLabelOnNodeByBinding('prod', DUMMY_INT_TYPE))
    .do(assertTypeLabelOnNodeByBinding('final', UNKNOWN_TYPE))
})
