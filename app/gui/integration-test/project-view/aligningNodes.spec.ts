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

  // Open alignment dropdown and click align left button
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignLeftButton = page.getByRole('button', { name: 'Align Left', exact: true })
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

  // Open alignment dropdown and click align right button
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignRightButton = page.getByRole('button', { name: 'Align Right', exact: true })
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

  // Open alignment dropdown and click align top button
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignTopButton = page.getByRole('button', { name: 'Align Top', exact: true })
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

  // Open alignment dropdown and click align bottom button
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignBottomButton = page.getByRole('button', { name: 'Align Bottom', exact: true })
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

  // Open alignment dropdown and click align center button
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignCenterButton = page.getByRole('button', { name: 'Center', exact: true })
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

  // Verify alignment dropdown button is present
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await expect(alignMenu).toBeVisible()

  // Open the dropdown and verify all alignment buttons are present
  await alignMenu.click()
  await expect(page.getByRole('button', { name: 'Align Left', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Align Right', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Align Top', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Align Bottom', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Center', exact: true })).toBeVisible()
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
  const alignMenu = selectionMenu.getByRole('button', { name: 'Align' })
  await alignMenu.click()
  const alignLeftButton = page.getByRole('button', { name: 'Align Left', exact: true })
  await alignLeftButton.click()

  // Get positions after left alignment
  const node1AfterLeft = await node1.boundingBox()
  const node2AfterLeft = await node2.boundingBox()
  assert(node1AfterLeft)
  assert(node2AfterLeft)

  // Then align top - need to reopen the dropdown
  await alignMenu.click()
  const alignTopButton = page.getByRole('button', { name: 'Align Top', exact: true })
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

test('Right-click on multiple nodes shows group context menu with alignment submenu', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })

  // Right-click on one of the selected nodes
  await locate.graphNodeIcon(node1).click({ button: 'right' })

  // Verify context menu appears with group-specific actions
  const contextMenu = page.locator('.ActionMenu')
  await expect(contextMenu).toBeVisible()

  // Verify alignment submenu option is present
  const alignSubmenu = contextMenu.getByRole('button', { name: 'Align' })
  await expect(alignSubmenu).toBeVisible()
})

test('Right-click on background deselects components', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Right-click on the background (viewport)
  const viewport = page.locator('.viewport')
  const viewportBox = await viewport.boundingBox()
  assert(viewportBox)
  // Click in an empty area far from any nodes
  await page.mouse.click(viewportBox.x + 50, viewportBox.y + 50, { button: 'right' })

  // Verify selection menu disappears (nodes are deselected)
  await expect(selectionMenu).toBeHidden()
})

test('Alignment actions work from context menu submenu on click', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')

  // Move node2 to ensure nodes have different x positions
  await editorPage.dragNode('ten', { x: 20, y: 0 })

  // Select both nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })

  // Get initial positions
  const node1InitialBBox = await node1.boundingBox()
  const node2InitialBBox = await node2.boundingBox()
  assert(node1InitialBBox)
  assert(node2InitialBBox)

  // Right-click to open context menu
  await locate.graphNodeIcon(node1).click({ button: 'right' })

  // Open alignment submenu
  const contextMenu = page.locator('.ActionMenu')
  const alignSubmenu = contextMenu.getByRole('button', { name: 'Align' })
  await alignSubmenu.click()

  // Click align left from the submenu
  const alignLeftButton = page.getByRole('button', { name: 'Align Left', exact: true })
  await expect(alignLeftButton).toBeVisible()
  await alignLeftButton.click()

  // Verify nodes are aligned
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const expectedX = Math.min(node1InitialBBox.x, node2InitialBBox.x)
  expect(node1NewBBox.x).toBeCloseTo(expectedX, 0)
  expect(node2NewBBox.x).toBeCloseTo(expectedX, 0)
})
