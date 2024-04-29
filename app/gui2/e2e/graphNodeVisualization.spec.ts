import { test } from '@playwright/test'
import assert from 'assert'
import * as actions from './actions'
import { computedContent } from './css'
import { expect } from './customExpect'
import * as locate from './locate'

test('node can open and load visualization', async ({ page }) => {
  await actions.goToGraph(page)
  const node = locate.graphNode(page).last()
  await node.click({ position: { x: 8, y: 8 } })
  await expect(locate.circularMenu(page)).toExist()
  await locate.toggleVisualizationButton(page).click()
  await expect(locate.anyVisualization(page)).toExist()
  await expect(locate.loadingVisualization(page)).toHaveCount(0)
  await locate.showVisualizationSelectorButton(page).click()
  await page.getByText('JSON').click()
  const vis = locate.jsonVisualization(page)
  await expect(vis).toExist()
  // The default JSON viz data contains an object.
  const element = await vis.elementHandle()
  assert(element != null)
  const textContent = await computedContent(element)
  const jsonContent = JSON.parse(textContent)
  expect(typeof jsonContent).toBe('object')
})

test('Warnings visualization', async ({ page }) => {
  await actions.goToGraph(page)

  // Create a node, attach a warning, open the warnings-visualization.
  await locate.addNewNodeButton(page).click()
  const input = locate.componentBrowserInput(page).locator('input')
  await input.fill('Warning.attach "Uh oh" 42')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).not.toBeVisible()
  await expect(locate.circularMenu(page)).toExist()
  await locate.toggleVisualizationButton(page).click()
  await expect(locate.anyVisualization(page)).toExist()
  await expect(locate.loadingVisualization(page)).toHaveCount(0)
  await locate.showVisualizationSelectorButton(page).click()
  await page.locator('.VisualizationSelector').getByRole('button', { name: 'Warnings' }).click()
  await expect(locate.warningsVisualization(page)).toExist()
  // Click the remove-warnings button, and ensure a node is created.
  const nodeCount = await locate.graphNode(page).count()
  await page.getByTestId('remove-warnings-button').click()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
})
