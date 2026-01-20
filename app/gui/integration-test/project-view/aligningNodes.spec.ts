import assert from 'assert'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'

test('Align Left button aligns multiple nodes to leftmost position', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Move node2 to ensure nodes have different x positions
  await editorPage.dragNode('ten', { x: 20, y: 0 })

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Click align left button
  const alignLeftButton = selectionMenu.getByLabel('Align Selected Components Left')
  await expect(alignLeftButton).toBeVisible()
  await alignLeftButton.click()

  // Verify nodes are aligned to leftmost x position
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const expectedX = Math.min(node1InitialBBox.x, node2InitialBBox.x)
  expect(node1NewBBox.x).toBeCloseTo(expectedX, 0)
  expect(node2NewBBox.x).toBeCloseTo(expectedX, 0)

  // Y positions should remain unchanged
  expect(node1NewBBox.y).toBeCloseTo(node1InitialBBox.y, 0)
  expect(node2NewBBox.y).toBeCloseTo(node2InitialBBox.y, 0)
})

test('Align Right button aligns multiple nodes to rightmost position', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Move node2 to ensure nodes have different x positions
  await editorPage.dragNode('sum', { x: 20, y: 0 })

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Click align right button
  const alignRightButton = selectionMenu.getByLabel('Align Selected Components Right')
  await expect(alignRightButton).toBeVisible()
  await alignRightButton.click()

  // Verify nodes are aligned to rightmost position
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const expectedRightEdge = Math.max(
    node1InitialBBox.x + node1InitialBBox.width,
    node2InitialBBox.x + node2InitialBBox.width,
  )
  expect(node1NewBBox.x + node1NewBBox.width).toBeCloseTo(expectedRightEdge, 0)
  expect(node2NewBBox.x + node2NewBBox.width).toBeCloseTo(expectedRightEdge, 0)

  // Y positions should remain unchanged
  expect(node1NewBBox.y).toBeCloseTo(node1InitialBBox.y, 0)
  expect(node2NewBBox.y).toBeCloseTo(node2InitialBBox.y, 0)
})

test('Align Top button aligns multiple nodes to topmost position', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Open visualization on node2 to make it taller
  await locate.graphNodeIcon(node2).click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(100)

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Click align top button
  const alignTopButton = selectionMenu.getByLabel('Align Selected Components Top')
  await expect(alignTopButton).toBeVisible()
  await alignTopButton.click()

  // Verify nodes are aligned to topmost y position
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const expectedY = Math.min(node1InitialBBox.y, node2InitialBBox.y)
  expect(node1NewBBox.y).toBeCloseTo(expectedY, 0)
  expect(node2NewBBox.y).toBeCloseTo(expectedY, 0)

  // X positions should remain unchanged
  expect(node1NewBBox.x).toBeCloseTo(node1InitialBBox.x, 0)
  expect(node2NewBBox.x).toBeCloseTo(node2InitialBBox.x, 0)
})

test('Align Bottom button aligns multiple nodes to bottommost position', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Open visualization on node2 to make it taller
  await locate.graphNodeIcon(node2).click()
  await page.keyboard.press('Space')
  await page.waitForTimeout(100)

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Click align bottom button
  const alignBottomButton = selectionMenu.getByLabel('Align Selected Components Bottom')
  await expect(alignBottomButton).toBeVisible()
  await alignBottomButton.click()
  await page.waitForTimeout(100)

  // Verify nodes are aligned to bottommost position
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  // Both nodes should have their bottom edges aligned
  const node1BottomEdge = node1NewBBox.y + node1NewBBox.height
  const node2BottomEdge = node2NewBBox.y + node2NewBBox.height + 150 // visualization height
  expect(node1BottomEdge).toBeCloseTo(node2BottomEdge, 0)

  // X positions should remain unchanged
  expect(node1NewBBox.x).toBeCloseTo(node1InitialBBox.x, 0)
  expect(node2NewBBox.x).toBeCloseTo(node2InitialBBox.x, 0)
})

test('Align Center button centers multiple nodes horizontally', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Move node2 to ensure nodes have different x positions
  await editorPage.dragNode('sum', { x: 20, y: 0 })

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Click align center button
  const alignCenterButton = selectionMenu.getByLabel('Center Selected Components Horizontally')
  await expect(alignCenterButton).toBeVisible()
  await alignCenterButton.click()

  // Verify nodes are centered horizontally around the average center
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const expectedCenterX =
    (node1InitialBBox.x +
      node1InitialBBox.width / 2 +
      node2InitialBBox.x +
      node2InitialBBox.width / 2) /
    2
  const node1NewCenterX = node1NewBBox.x + node1NewBBox.width / 2
  const node2NewCenterX = node2NewBBox.x + node2NewBBox.width / 2

  expect(node1NewCenterX).toBeCloseTo(expectedCenterX, 0)
  expect(node2NewCenterX).toBeCloseTo(expectedCenterX, 0)

  // Y positions should remain unchanged
  expect(node1NewBBox.y).toBeCloseTo(node1InitialBBox.y, 0)
  expect(node2NewBBox.y).toBeCloseTo(node2InitialBBox.y, 0)
})

test('Alignment buttons are hidden when only one node is selected', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'five')
  const selectionMenu = page.locator('.SelectionMenu')

  // Select only one node - selection menu should not appear
  await locate.graphNodeIcon(node).click()
  await expect(selectionMenu).toBeHidden()
})

test('Alignment buttons are visible when multiple nodes are selected', async ({
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

  // Verify all alignment buttons are present
  await expect(selectionMenu.getByLabel('Align Selected Components Left')).toBeVisible()
  await expect(selectionMenu.getByLabel('Align Selected Components Right')).toBeVisible()
  await expect(selectionMenu.getByLabel('Align Selected Components Top')).toBeVisible()
  await expect(selectionMenu.getByLabel('Align Selected Components Bottom')).toBeVisible()
  await expect(selectionMenu.getByLabel('Center Selected Components Horizontally')).toBeVisible()
})

test('Multiple alignment operations can be performed sequentially', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Move node2 to ensure nodes have different x positions
  await editorPage.dragNode('sum', { x: 20, y: 0 })

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // First align left
  const alignLeftButton = selectionMenu.getByLabel('Align Selected Components Left')
  await alignLeftButton.click()

  // Get positions after left alignment
  const node1AfterLeft = await node1.boundingBox()
  const node2AfterLeft = await node2.boundingBox()
  assert(node1AfterLeft)
  assert(node2AfterLeft)

  // Then align top
  const alignTopButton = selectionMenu.getByLabel('Align Selected Components Top')
  await alignTopButton.click()

  // Verify both alignments took effect
  const node1Final = await node1.boundingBox()
  const node2Final = await node2.boundingBox()
  assert(node1Final)
  assert(node2Final)

  // Both nodes should be at the same x position (from align left)
  expect(node1Final.x).toBeCloseTo(node2Final.x, 0)

  // Both nodes should be at the same y position (from align top)
  expect(node1Final.y).toBeCloseTo(node2Final.y, 0)
})
