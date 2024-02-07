import { expect, Page, test } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'
import { edgesToNodeWithBinding, enterFullscreenButton, graphNodeByBinding } from './locate'

/**
 * Prepare the graph for the tests.
 */
async function initGraph(page: Page) {
  await actions.goToGraph(page)
}

/**
 Scenario: We open the default visualisation of the `aggregated` node. We then make it fullscreen and expect it to show
 the JSON data of the node. We also expect it to cover the whole screen and to have a button to exit fullscreen mode.
 */
test('Load Fullscreen Visualisation', async ({ page }) => {
  await initGraph(page)

  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const fullscreenButton = locate.enterFullscreenButton(aggregatedNode)
  await fullscreenButton.click()
  const vis = locate.jsonVisualization(page)
  await customExpect.toExist(vis)
  await customExpect.toExist(locate.exitFullscreenButton(page))
  const visBoundingBox = await vis.boundingBox()
  expect(visBoundingBox!.height).toBe(808)
  expect(visBoundingBox!.width).toBe(1920)
  await expect(vis).toContainText(
    '{\n' +
      '  "axis": {\n' +
      '    "x": {\n' +
      '      "label": "x-axis label",\n' +
      '      "scale": "linear"\n' +
      '    },\n' +
      '    "y": {\n' +
      '      "label": "y-axis label",\n' +
      '      "scale": "logarithmic"\n' +
      '    }\n' +
      '  },\n' +
      '  "points": {\n' +
      '    "labels": "visible"\n' +
      '  },\n' +
      '  "data": [\n' +
      '    {\n' +
      '      "x": 0.1,\n' +
      '      "y": 0.7,\n' +
      '      "label": "foo",\n' +
      '      "color": "#FF0000",\n' +
      '      "shape": "circle",\n' +
      '      "size": 0.2\n' +
      '    },\n' +
      '    {\n' +
      '      "x": 0.4,\n' +
      '      "y": 0.2,\n' +
      '      "label": "baz",\n' +
      '      "color": "#0000FF",\n' +
      '      "shape": "square",\n' +
      '      "size": 0.3\n' +
      '    }\n' +
      '  ]\n' +
      '}',
  )
})
