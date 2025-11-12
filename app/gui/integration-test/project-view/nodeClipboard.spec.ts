import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import * as locate from './locate'

test('Copy component with context menu', async ({ editorPage, page }) => {
  await editorPage
  const originalNodes = await editorPage.nodeCount()
  const nodeToCopy = editorPage.locateNodes('final')
  await nodeToCopy.click({ button: 'right' })
  await expect(nodeToCopy).toBeSelected()
  await page.locator('.ActionMenu').getByRole('button', { name: 'Copy Component' }).click()
  await expect(page.locator('.ActionMenu')).toBeHidden()
  await editorPage.press('Mod+V')
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 1)
  await editorPage.expectSelectedNodesExactly(['final1'])
})

test('Copy component with comment', async ({ editorPage, page }) => {
  await editorPage

  // Remember state before operation.
  await expect(locate.nodeCommentContent(page)).toExist()
  const originalNodes = await editorPage.nodeCount()
  const originalNodeComments = await locate.nodeCommentContent(page).count()

  // Select a node.
  await editorPage.selectSingleNode('final')
  // Copy and paste it.
  await editorPage.press('Mod+C')
  await editorPage.press('Mod+V')
  await editorPage.expectNodeCount(1, '.selected')

  // Node and comment have been copied.
  await expect(locate.graphNode(page)).toHaveCount(originalNodes + 1)
  await expect(locate.nodeCommentContent(page)).toHaveCount(originalNodeComments + 1)
})

function testCopyMultiple(editorPage: EditorPageActions, copyNodes: () => Promise<void>) {
  let originalNodes = 0
  let originalNodeComments = 0
  return (
    editorPage
      .do(async (page) => {
        // Check state before operation.
        await expect(locate.nodeCommentContent(page)).toExist()
        originalNodes = await locate.graphNode(page).count()
        originalNodeComments = await locate.nodeCommentContent(page).count()
      })
      // Select some nodes.
      .selectNodes(['final', 'prod'])
      .do(copyNodes)
      // `final` node has a comment, expect it to have been copied.
      // Nodes have been copied.
      .defer((p) => p.expectNodeCount(originalNodes + 2))
      .do((page) => expect(locate.nodeCommentContent(page)).toHaveCount(originalNodeComments + 1))
      .expectNodesToExist(['prod1', 'final1'])
      .expectSelectedNodesExactly(['prod1', 'final1'])
      // Check that two copied nodes are isolated, i.e. connected to each other, not original nodes.
      .expectEdgesFromTo('sum', undefined, 2)
      .expectEdgesFromTo('prod', undefined, 1)
      .expectEdgesFromTo(undefined, 'prod', 1)
      .expectEdgesFromTo(undefined, 'final', 1)
      .expectEdgesFromTo(undefined, 'prod1', 1)
      .expectEdgesFromTo(undefined, 'final1', 1)
  )
}

test('Copy multiple components with keyboard shortcut', async ({ editorPage }) => {
  await testCopyMultiple(editorPage, async () => {
    await editorPage.press('Mod+C')
    await editorPage.press('Mod+V')
  })
})

test('Copy multiple components with context menu', async ({ editorPage }) => {
  await testCopyMultiple(editorPage, async () => {
    await editorPage.locateNodes('.selected').first().click({ button: 'right' })
    await editorPage.clickActionTrigger('components.copy', true).press('Mod+V')
  })
})
