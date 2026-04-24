import { expect, test, type Page } from 'integration-test/base'
import { DELETE_KEY } from './keyboard'
import * as locate from './locate'

async function createNode(page: Page, expression: string) {
  const nodesCount = await locate.graphNode(page).count()
  const newNodesCount = nodesCount + 1
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserInput(page)).toBeFocused()
  await page.keyboard.insertText(expression)
  await page.keyboard.press(`ControlOrMeta+Enter`)
  await expectNodeCreated(page, newNodesCount, expression)
  return newNodesCount
}

async function expectNodeCreated(page: Page, expectedNodesCount: number, expression: string) {
  await expect(locate.graphNode(page)).toHaveCount(expectedNodesCount)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([expression])
  await expect(page.locator('[data-transitioning]')).toHaveCount(0)
}

test('Undo/redo buttons work', async ({ editorPage, page }) => {
  await editorPage
  const undoButton = page.getByTestId('action:graph.undo')
  const redoButton = page.getByTestId('action:graph.redo')

  const nodesCount = await createNode(page, 'foo')

  await undoButton.click()
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
  await expect(
    locate.graphNode(page).locator('.WidgetToken').filter({ hasText: 'foo' }),
  ).toHaveCount(0)

  await redoButton.click()
  await expectNodeCreated(page, nodesCount, 'foo')
})

test('Adding new node', async ({ editorPage, page }) => {
  await editorPage

  const nodesCount = await createNode(page, 'foo')
  const newNodeBBox = await locate.graphNode(page).last().boundingBox()

  await page.keyboard.press(`ControlOrMeta+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
  await expect(
    locate.graphNode(page).locator('.WidgetToken').filter({ hasText: 'foo' }),
  ).toHaveCount(0)

  await page.keyboard.press(`ControlOrMeta+Shift+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText(['foo'])
  const restoredBox = await locate.graphNode(page).last().boundingBox()
  expect(restoredBox).toEqual(newNodeBBox)
})

test('Removing node', async ({ editorPage, page }) => {
  await editorPage

  const nodesCount = await locate.graphNode(page).count()
  const deletedNode = locate.graphNodeByBinding(page, 'final')
  const deletedNodeBBox = await deletedNode.boundingBox()
  await deletedNode.locator('.grab-handle').click()
  await page.keyboard.press(DELETE_KEY)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)

  await page.keyboard.press(`ControlOrMeta+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount)
  await expect(deletedNode.locator('.WidgetToken')).toHaveText(['func1'])
  await expect(locate.nodeCommentContent(deletedNode)).toHaveText('This node can be entered')

  const restoredBBox = await deletedNode.boundingBox()
  expect(restoredBBox).toEqual(deletedNodeBBox)

  await page.keyboard.press(`ControlOrMeta+Shift+Z`)
  await expect(locate.graphNode(page)).toHaveCount(nodesCount - 1)
  await expect(deletedNode).toBeHidden()
})
