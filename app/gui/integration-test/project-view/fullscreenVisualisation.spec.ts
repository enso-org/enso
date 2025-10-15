import assert from 'assert'
import { expect, test } from 'integration-test/base'
import { computedContent } from './css'
import * as locate from './locate'
import { graphNodeByBinding } from './locate'

/**
 Scenario: We open the default visualisation of the `aggregated` node. We then make it fullscreen and expect it to show
 the JSON data of the node. We also expect it to cover the whole screen and to have a button to exit fullscreen mode.
 */
test('Load Fullscreen Visualisation', async ({ editorPage, page }) => {
  await editorPage
  const aggregatedNode = graphNodeByBinding(page, 'aggregated')
  await aggregatedNode.click()
  await page.keyboard.press('Space')
  const vis = locate.jsonVisualization(page)
  await expect(vis).toExist()
  const initialBBox = await vis.boundingBox()
  assert(initialBBox != null)
  const fullscreenButton = locate.enterFullscreenButton(aggregatedNode)
  await expect(fullscreenButton).toBeVisible()
  await fullscreenButton.click()

  await expect(locate.exitFullscreenButton(page)).toExist()
  // Wait for entering-fullscreen animation.
  await expect.poll(async () => (await vis.boundingBox())?.width).toBe(1920)
  await expect.poll(async () => (await vis.boundingBox())?.height).toBeGreaterThan(600)
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

  // We can switch visualization type to Table
  await locate.toggleVisualizationSelectorButton(page).click()
  await page.getByRole('button', { name: 'Table', exact: true }).click()
  await expect(locate.tableVisualization(page)).toExist()
  // ... and back to JSON
  await locate.toggleVisualizationSelectorButton(page).click()
  await page.getByRole('button', { name: 'JSON', exact: true }).click()
  await expect(locate.jsonVisualization(page)).toExist()
  await expect(page.getByRole('button', { name: 'JSON', exact: true })).toBeHidden()

  // We may leave fullscreen by pressing Escape
  await page.keyboard.press('Escape')

  await expect.poll(() => vis.boundingBox()).toEqual(initialBBox)
})
