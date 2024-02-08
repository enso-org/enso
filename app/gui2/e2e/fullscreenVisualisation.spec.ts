import { expect, test, type Page } from '@playwright/test'
import * as actions from './actions'
import * as customExpect from './customExpect'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

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
  const jsonContent = await vis.textContent().then((text) => JSON.parse(text!))
  expect(jsonContent).toEqual({
    axis: {
      x: {
        label: 'x-axis label',
        scale: 'linear',
      },
      y: {
        label: 'y-axis label',
        scale: 'logarithmic',
      },
    },
    points: {
      labels: 'visible',
    },
    data: [
      {
        x: 0.1,
        y: 0.7,
        label: 'foo',
        color: '#FF0000',
        shape: 'circle',
        size: 0.2,
      },
      {
        x: 0.4,
        y: 0.2,
        label: 'baz',
        color: '#0000FF',
        shape: 'square',
        size: 0.3,
      },
    ],
  })
})
