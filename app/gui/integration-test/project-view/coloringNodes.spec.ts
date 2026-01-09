import assert from 'assert'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'

test('Color picker button appears when multiple nodes are selected', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Verify color picker button is present
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await expect(colorPickerButton).toBeVisible()
})

test('Color picker menu opens when clicking the color button', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Verify color picker menu is initially hidden
  await expect(colorPickerMenu).toBeHidden()

  // Click the color picker button
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await colorPickerButton.click()
  await page.waitForTimeout(100)

  // Verify color picker menu is now visible
  await expect(colorPickerMenu).toBeVisible()
})

test('Multiple nodes can be colored with the color picker', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial node styles (to check they don't have explicit color overrides initially)
  const node1InitialStyle = await node1.getAttribute('style')
  const node2InitialStyle = await node2.getAttribute('style')

  // Click the color picker button
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await colorPickerButton.click()
  await page.waitForTimeout(100)
  await expect(colorPickerMenu).toBeVisible()

  // Click on a color in the color ring (click in the center-right area of the color ring)
  const colorRing = colorPickerMenu.locator('.gradient')
  await expect(colorRing).toBeVisible()
  const colorRingBox = await colorRing.boundingBox()
  assert(colorRingBox)
  // Move to the right side of the ring (roughly 3 o'clock position) to trigger color change
  const targetX = colorRingBox.x + colorRingBox.width * 0.85
  const targetY = colorRingBox.y + colorRingBox.height / 2
  await page.mouse.move(targetX, targetY)
  await page.waitForTimeout(50)
  // Click to finalize and close
  await page.mouse.click(targetX, targetY)
  await page.waitForTimeout(100)

  // Get new node styles - they should now have color overrides
  const node1NewStyle = await node1.getAttribute('style')
  const node2NewStyle = await node2.getAttribute('style')

  // Both nodes should have received color overrides
  expect(node1NewStyle).not.toEqual(node1InitialStyle)
  expect(node2NewStyle).not.toEqual(node2InitialStyle)

  // Both nodes should have the same color applied (they should contain the same color variable)
  expect(node1NewStyle).toContain('--node-group-color')
  expect(node2NewStyle).toContain('--node-group-color')
})

test('Color picker can be closed by clicking outside', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Open the color picker
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await colorPickerButton.click()
  await page.waitForTimeout(100)
  await expect(colorPickerMenu).toBeVisible()

  // Click outside the color picker (on the graph editor)
  await page.click('.GraphEditor', { position: { x: 50, y: 50 } })
  await page.waitForTimeout(100)

  // Verify color picker menu is now hidden
  await expect(colorPickerMenu).toBeHidden()
})

test('Color picker opens with keyboard shortcut', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Verify color picker menu is initially hidden
  await expect(colorPickerMenu).toBeHidden()

  // Use keyboard shortcut (ControlOrMeta+Shift+C)
  await page.keyboard.press('ControlOrMeta+Shift+C')
  await page.waitForTimeout(100)

  // Verify color picker menu is now visible
  await expect(colorPickerMenu).toBeVisible()
})

test('Color picker is not available when only one node is selected', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'five')
  const selectionMenu = page.locator('.SelectionMenu')

  // Select only one node
  await locate.graphNodeIcon(node).click()

  // Selection menu should not appear (only appears for multiple selections)
  await expect(selectionMenu).toBeHidden()

  // Try using the keyboard shortcut - it should not open the color picker
  const colorPickerMenu = page.locator('.ColorPickerMenu')
  await page.keyboard.press('ControlOrMeta+Shift+C')
  await page.waitForTimeout(100)
  await expect(colorPickerMenu).toBeHidden()
})

test('Multiple color changes can be applied to the same selection', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Open the color picker
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await colorPickerButton.click()
  await page.waitForTimeout(100)
  await expect(colorPickerMenu).toBeVisible()

  // Select first color (right side of ring)
  const colorRing = colorPickerMenu.locator('.gradient')
  const colorRingBox = await colorRing.boundingBox()
  assert(colorRingBox)
  const firstX = colorRingBox.x + colorRingBox.width * 0.85
  const firstY = colorRingBox.y + colorRingBox.height / 2
  await page.mouse.move(firstX, firstY)
  await page.waitForTimeout(100)

  const node1FirstColor = await node1.getAttribute('style')
  const node2FirstColor = await node2.getAttribute('style')

  // Move to a different color (bottom of ring)
  const secondX = colorRingBox.x + colorRingBox.width / 2
  const secondY = colorRingBox.y + colorRingBox.height * 0.85
  await page.mouse.move(secondX, secondY)
  await page.waitForTimeout(100)

  const node1SecondColor = await node1.getAttribute('style')
  const node2SecondColor = await node2.getAttribute('style')

  // Verify that the colors changed
  expect(node1SecondColor).not.toEqual(node1FirstColor)
  expect(node2SecondColor).not.toEqual(node2FirstColor)

  // Both nodes should still have color overrides
  expect(node1SecondColor).toContain('--node-group-color')
  expect(node2SecondColor).toContain('--node-group-color')
})

test('Colored nodes retain their color after deselection', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')
  const colorPickerMenu = page.locator('.ColorPickerMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Open the color picker and apply a color
  const colorPickerButton = selectionMenu.getByLabel('Color Selected Components')
  await colorPickerButton.click()
  await page.waitForTimeout(100)
  await expect(colorPickerMenu).toBeVisible()

  const colorRing = colorPickerMenu.locator('.gradient')
  const colorRingBox = await colorRing.boundingBox()
  assert(colorRingBox)
  const targetX = colorRingBox.x + colorRingBox.width * 0.85
  const targetY = colorRingBox.y + colorRingBox.height / 2
  await page.mouse.move(targetX, targetY)
  await page.waitForTimeout(50)
  await page.mouse.click(targetX, targetY)
  await page.waitForTimeout(100)

  // Get the colored styles
  const node1ColoredStyle = await node1.getAttribute('style')
  const node2ColoredStyle = await node2.getAttribute('style')

  // Close the color picker by clicking outside
  await page.click('.GraphEditor', { position: { x: 50, y: 50 } })
  await page.waitForTimeout(100)

  // Deselect nodes by clicking on empty space
  await page.click('.GraphEditor', { position: { x: 100, y: 100 } })
  await page.waitForTimeout(100)

  // Verify the nodes still have the same color
  const node1FinalStyle = await node1.getAttribute('style')
  const node2FinalStyle = await node2.getAttribute('style')

  expect(node1FinalStyle).toEqual(node1ColoredStyle)
  expect(node2FinalStyle).toEqual(node2ColoredStyle)
})
