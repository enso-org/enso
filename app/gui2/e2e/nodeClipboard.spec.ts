import test from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'

test.beforeEach(async ({ page }) => {
  await page.addInitScript(() => {
    class MockClipboard {
      private contents: ClipboardItem[] = []
      async read(): Promise<ClipboardItem[]> {
        return [...this.contents]
      }
      async write(contents: ClipboardItem[]) {
        this.contents = [...contents]
      }
    }
    Object.assign(window.navigator, {
      mockClipboard: new MockClipboard(),
    })
  })
})

test('Copy node with comment', async ({ page }) => {
  await actions.goToGraph(page)

  // Check state before operation.
  const originalNodes = await locate.graphNode(page).count()
  await expect(page.locator('.GraphNodeComment')).toExist()
  const originalNodeComments = await page.locator('.GraphNodeComment').count()

  // Select a node.
  const nodeToCopy = locate.graphNodeByBinding(page, 'final')
  await nodeToCopy.click()
  await expect(nodeToCopy).toBeSelected()
  // Copy and paste it.
  await page.keyboard.press(`${CONTROL_KEY}+C`)
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  await expect(nodeToCopy).not.toBeSelected()
  await expect(locate.selectedNodes(page)).toHaveCount(1)

  // Node and comment have been copied.
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 1)
  await expect(page.locator('.GraphNodeComment')).toHaveCount(originalNodeComments + 1)
})

test('Copy multiple nodes', async ({ page }) => {
  await actions.goToGraph(page)

  // Check state before operation.
  const originalNodes = await locate.graphNode(page).count()
  await expect(page.locator('.GraphNodeComment')).toExist()
  const originalNodeComments = await page.locator('.GraphNodeComment').count()

  // Select some nodes.
  const node1 = locate.graphNodeByBinding(page, 'final')
  await node1.click()
  const node2 = locate.graphNodeByBinding(page, 'data')
  await node2.click({ modifiers: ['Shift'] })
  await expect(node1).toBeSelected()
  await expect(node2).toBeSelected()
  // Copy and paste.
  await page.keyboard.press(`${CONTROL_KEY}+C`)
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  await expect(node1).not.toBeSelected()
  await expect(node2).not.toBeSelected()
  await expect(locate.selectedNodes(page)).toHaveCount(2)

  // Nodes and comment have been copied.
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 2)
  await expect(page.locator('.GraphNodeComment')).toHaveCount(originalNodeComments + 1)
})
