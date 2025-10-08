import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'

const DUMMY_INT_TYPE = { full: 'Standard.Base.Data.Numbers.Integer', short: 'Integer' }
const DUMMY_STRING_TYPE = { full: 'Standard.Base.Data.Text.Text', short: 'Text' }
const DUMMY_FLOAT_TYPE = { full: 'Standard.Base.Data.Numbers.Float', short: 'Float' }
const UNKNOWN_TYPE = { full: 'Unknown', short: 'Unknown' }

function assertTypeLabelOnNodeByBinding(label: string, type: { full: string; short: string }) {
  return (editorPage: EditorPageActions) =>
    editorPage
      .selectSingleNode(label)
      .press('Escape')
      .withNode(label, async (node) => {
        await node.hover({ position: { x: 8, y: 8 }, force: true })
        await locate.toggleVisualizationButton(node).click({ force: true })
        await expect(node.locator('.node-type').first()).toHaveText(type.short)
        await locate.toggleVisualizationButton(node).click({ force: true })
      })
      .clearSelection()
}

test('shows the correct type when hovering a node', async ({ editorPage }) => {
  await editorPage
    // Note that the types don't have to make sense, they just have to be applied.
    .mockExpressionUpdate('five', { type: [DUMMY_INT_TYPE.full] })
    .mockExpressionUpdate('ten', { type: [DUMMY_STRING_TYPE.full] })
    .mockExpressionUpdate('sum', { type: [DUMMY_FLOAT_TYPE.full] })
    .mockExpressionUpdate('prod', { type: [DUMMY_INT_TYPE.full] })
    .call(assertTypeLabelOnNodeByBinding('five', DUMMY_INT_TYPE))
    .call(assertTypeLabelOnNodeByBinding('ten', DUMMY_STRING_TYPE))
    .call(assertTypeLabelOnNodeByBinding('sum', DUMMY_FLOAT_TYPE))
    .call(assertTypeLabelOnNodeByBinding('prod', DUMMY_INT_TYPE))
    .call(assertTypeLabelOnNodeByBinding('final', UNKNOWN_TYPE))
})
