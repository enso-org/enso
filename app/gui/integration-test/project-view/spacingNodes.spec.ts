import assert from 'assert'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'

test('Space Default button sets default spacing between nodes', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

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

  // Open spacing dropdown and click default spacing button
  const spacingMenu = selectionMenu.getByRole('button', { name: 'Spacing' })
  await spacingMenu.click()
  const spaceDefaultButton = page.getByRole('button', { name: 'Default Spacing', exact: true })
  await expect(spaceDefaultButton).toBeVisible()
  await spaceDefaultButton.click()

  // Verify nodes have default spacing applied
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  // Calculate the spacing between nodes (bottom of node1 to top of node2)
  const spacing = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  // Default spacing should be approximately 40px
  expect(spacing).toBeCloseTo(40, 5)
})

test('Space Tight button sets tight spacing between nodes', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

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

  // Open spacing dropdown and click tight spacing button
  const spacingMenu = selectionMenu.getByRole('button', { name: 'Spacing' })
  await spacingMenu.click()
  const spaceTightButton = page.getByRole('button', { name: 'Tight Spacing', exact: true })
  await expect(spaceTightButton).toBeVisible()
  await spaceTightButton.click()

  // Verify nodes have tight spacing applied
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  // Calculate the spacing between nodes
  const spacing = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  // Tight spacing should be approximately 20px
  expect(spacing).toBeCloseTo(20, 5)
})

test('Space Zero button sets zero spacing between nodes', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

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

  // Open spacing dropdown and click zero spacing button
  const spacingMenu = selectionMenu.getByRole('button', { name: 'Spacing' })
  await spacingMenu.click()
  const spaceZeroButton = page.getByRole('button', { name: 'Zero Spacing', exact: true })
  await expect(spaceZeroButton).toBeVisible()
  await spaceZeroButton.click()

  // Verify nodes have zero spacing applied
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  // Calculate the spacing between nodes
  const spacing = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  // Zero spacing should be very close to 0px (allowing 1-2px tolerance for rounding)
  expect(spacing).toBeLessThanOrEqual(2)
})

test('Space Wide button sets wide spacing between nodes', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

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

  // Open spacing dropdown and click wide spacing button
  const spacingMenu = selectionMenu.getByRole('button', { name: 'Spacing' })
  await spacingMenu.click()
  const spaceWideButton = page.getByRole('button', { name: 'Wide Spacing', exact: true })
  await expect(spaceWideButton).toBeVisible()
  await spaceWideButton.click()

  // Verify nodes have wide spacing applied
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  // Calculate the spacing between nodes
  const spacing = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  // Wide spacing should be approximately 80px
  expect(spacing).toBeCloseTo(80, 5)
})

test('Spacing buttons work with more than two nodes', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'ten')
  const node3 = locate.graphNodeByBinding(page, 'sum')
  const selectionMenu = page.locator('.SelectionMenu')

  // Select all three nodes
  await locate.graphNodeIcon(node1).click()
  await page.waitForTimeout(300)
  await locate.graphNodeIcon(node2).click({ modifiers: ['Shift'] })
  await locate.graphNodeIcon(node3).click({ modifiers: ['Shift'] })
  await expect(selectionMenu).toBeVisible()

  // Open spacing dropdown and click tight spacing button
  const spacingMenu = selectionMenu.getByRole('button', { name: 'Spacing' })
  await spacingMenu.click()
  const spaceTightButton = page.getByRole('button', { name: 'Tight Spacing', exact: true })
  await expect(spaceTightButton).toBeVisible()
  await spaceTightButton.click()

  // Verify all nodes have tight spacing applied between them
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  const node3NewBBox = await node3.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)
  assert(node3NewBBox)

  // Calculate spacing between consecutive nodes
  const spacing1to2 = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  const spacing2to3 = node3NewBBox.y - (node2NewBBox.y + node2NewBBox.height)

  // Both spacings should be approximately 20px for tight spacing
  expect(spacing1to2).toBeCloseTo(20, 5)
  expect(spacing2to3).toBeCloseTo(20, 5)
})

test('Spacing actions work from context menu submenu', async ({ editorPage, page }) => {
  await editorPage
  const node1 = locate.graphNodeByBinding(page, 'five')
  const node2 = locate.graphNodeByBinding(page, 'sum')

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

  // Open spacing submenu
  const contextMenu = page.locator('.ActionMenu')
  const spacingSubmenu = contextMenu.getByRole('button', { name: 'Spacing' })
  await spacingSubmenu.click()

  // Click wide spacing from the submenu
  const spaceWideButton = page.getByRole('button', { name: 'Wide Spacing', exact: true })
  await expect(spaceWideButton).toBeVisible()
  await spaceWideButton.click()

  // Verify wide spacing is applied
  const node1NewBBox = await node1.boundingBox()
  const node2NewBBox = await node2.boundingBox()
  assert(node1NewBBox)
  assert(node2NewBBox)

  const spacing = node2NewBBox.y - (node1NewBBox.y + node1NewBBox.height)
  expect(spacing).toBeCloseTo(80, 5)
})
