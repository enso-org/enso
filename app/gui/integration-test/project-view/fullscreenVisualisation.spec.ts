import { test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { computedContent } from './css'
import { expect } from './customExpect'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

/**
 Scenario: We open the default visualisation of the `aggregated` node. We then make it fullscreen and expect it to show
 the JSON data of the node. We also expect it to cover the whole screen and to have a button to exit fullscreen mode.
 */
test('Load Fullscreen Visualisation', async ({ page }) => {
  await actions.goToGraph(page)
  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(1000)
  const fullscreenButton = locate.enterFullscreenButton(aggregatedNode)
  await fullscreenButton.click()
  const vis = locate.jsonVisualization(page)
  await expect(vis).toExist()
  await expect(locate.exitFullscreenButton(page)).toExist()
  // Wait for entering-fullscreen animation.
  await vis.elementHandle().then((el) => el!.waitForElementState('stable'))
  const visBoundingBox = await vis.boundingBox()
  expect(visBoundingBox?.height).toBeGreaterThan(600)
  expect(visBoundingBox?.width).toBe(1920)
  const element = await vis.elementHandle()
  assert(element != null)
  const textContent = await computedContent(element)
  const jsonContent = JSON.parse(textContent)
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
