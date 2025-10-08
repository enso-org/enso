import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test, type Locator } from 'integration-test/base'
import * as locate from './locate'
import { connectedEdgesFromNodeWithBinding, edgesToNodeWithBinding } from './locate'

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

test('Copy component with context menu', async ({ editorPage, page }) => {
  await editorPage
  const originalNodes = await locate.graphNode(page).count()
  const nodeToCopy = locate.graphNodeByBinding(page, 'final')
  await nodeToCopy.click({ button: 'right' })
  await expect(nodeToCopy).toBeSelected()
  await page.locator('.ActionMenu').getByRole('button', { name: 'Copy Component' }).click()
  await page.keyboard.press(`ControlOrMeta+V`)
  await expect(nodeToCopy).not.toBeSelected()
  await expect(locate.selectedNodes(page)).toHaveCount(1)
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 1)
})

test('Copy component with comment', async ({ editorPage, page }) => {
  await editorPage

  // Check state before operation.
  const originalNodes = await locate.graphNode(page).count()
  await expect(locate.nodeCommentContent(page)).toExist()
  const originalNodeComments = await locate.nodeCommentContent(page).count()

  // Select a node.
  const nodeToCopy = locate.graphNodeByBinding(page, 'final')
  await nodeToCopy.click()
  await expect(nodeToCopy).toBeSelected()
  // Copy and paste it.
  await page.keyboard.press(`ControlOrMeta+C`)
  await page.keyboard.press(`ControlOrMeta+V`)
  await expect(nodeToCopy).not.toBeSelected()
  await expect(locate.selectedNodes(page)).toHaveCount(1)

  // Node and comment have been copied.
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 1)
  await expect(locate.nodeCommentContent(page)).toHaveCount(originalNodeComments + 1)
})

async function testCopyMultiple(
  editorPage: EditorPageActions,
  copyNodes: (node1: Locator, node2: Locator) => Promise<void>,
) {
  await editorPage.do(async (page) => {
    // Check state before operation.
    const originalNodes = await locate.graphNode(page).count()
    await expect(locate.nodeCommentContent(page)).toExist()
    const originalNodeComments = await locate.nodeCommentContent(page).count()

    // Select some nodes.
    const node1 = locate.graphNodeByBinding(page, 'final')
    const node2 = locate.graphNodeByBinding(page, 'prod')

    // Copy and paste.
    await copyNodes(node1, node2)
    await page.keyboard.press(`ControlOrMeta+V`)
    await expect(node1).not.toBeSelected()
    await expect(node2).not.toBeSelected()
    await expect(locate.selectedNodes(page)).toHaveCount(2)

    // Nodes and comment have been copied.
    await expect(locate.graphNode(page)).toHaveCount(originalNodes + 2)
    // `final` node has a comment.
    await expect(locate.nodeCommentContent(page)).toHaveCount(originalNodeComments + 1)
    // Check that two copied nodes are isolated, i.e. connected to each other, not original nodes.
    await expect(locate.graphNodeByBinding(page, 'prod1')).toBeVisible()
    await expect(locate.graphNodeByBinding(page, 'final1')).toBeVisible()
    await expect(await connectedEdgesFromNodeWithBinding(page, 'sum')).toHaveCount(2)
    await expect(await connectedEdgesFromNodeWithBinding(page, 'prod')).toHaveCount(1)

    await expect(await edgesToNodeWithBinding(page, 'prod')).toHaveCount(1)
    await expect(await edgesToNodeWithBinding(page, 'final')).toHaveCount(1)
    await expect(await edgesToNodeWithBinding(page, 'prod1')).toHaveCount(1)
    await expect(await edgesToNodeWithBinding(page, 'final1')).toHaveCount(1)
  })
}

test('Copy multiple components with keyboard shortcut', async ({ editorPage }) => {
  await testCopyMultiple(editorPage, async (node1, node2) => {
    await node1.click()
    await node2.click({ modifiers: ['Shift'] })
    await expect(node1).toBeSelected()
    await expect(node2).toBeSelected()
    await editorPage.press('Mod+C')
  })
})

test('Copy multiple components with context menu', async ({ editorPage }) => {
  await testCopyMultiple(editorPage, async (node1, node2) => {
    await node1.click()
    await node2.click({ modifiers: ['Shift'] })
    await expect(node1).toBeSelected()
    await expect(node2).toBeSelected()
    await node1.click({ button: 'right' })
    await editorPage.clickActionTrigger('components.copy')
  })
})
