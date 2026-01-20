import { expect, test } from 'integration-test/base'
import * as locate from './locate'

test('Edit comment by click', async ({ editorPage, page }) => {
  await editorPage
  const nodeComment = locate.nodeCommentContent(locate.graphNodeByBinding(page, 'final'))
  await expect(nodeComment).toHaveText('This node can be entered')

  await nodeComment.click()
  await page.keyboard.press(`ControlOrMeta+A`)
  const NEW_COMMENT = 'New comment text'
  await nodeComment.fill(NEW_COMMENT)
  await page.keyboard.press(`Enter`)
  await expect(nodeComment).not.toBeFocused()
  await expect(nodeComment).toHaveText(NEW_COMMENT)
})

test('Start editing comment via menu', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'final')
  await node.click()
  await locate.componentMenu(node).getByRole('button', { name: 'More' }).click()
  await locate.componentMenuMoreEntries(node).getByRole('button', { name: 'Comment' }).click()
  await expect(locate.nodeCommentContent(node)).toBeFocused()
})

test('Start editing comment via context menu', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'final')
  await node.click({ button: 'right' })
  await page.getByRole('button', { name: 'Add Comment' }).click()
  await expect(locate.nodeCommentContent(node)).toBeFocused()
})

test('Start editing comment via context menu when multiple components initially selected', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const otherNode = locate.graphNodeByBinding(page, 'sum')
  await otherNode.click()
  const node = locate.graphNodeByBinding(page, 'final')
  await node.click({ modifiers: ['Shift'] })
  const anotherNode = locate.graphNodeByBinding(page, 'list')
  await anotherNode.click({ modifiers: ['Shift'] })
  await node.click({ button: 'right' })
  await expect(locate.selectedNodes(page)).toHaveCount(3)
  await page.getByRole('button', { name: 'Add Comment' }).click()
  await expect(locate.selectedNodes(page)).toHaveCount(1)
  await expect(locate.nodeCommentContent(node)).toBeFocused()
})

test('Add new comment via menu', async ({ editorPage, page }) => {
  await editorPage
  const INITIAL_NODE_COMMENTS = 1
  await expect(locate.nodeCommentContent(page)).toHaveCount(INITIAL_NODE_COMMENTS)
  const node = locate.graphNodeByBinding(page, 'data')
  const nodeComment = locate.nodeCommentContent(node)

  await node.click()
  await locate.componentMenu(node).getByRole('button', { name: 'More' }).click()
  await locate.componentMenuMoreEntries(node).getByRole('button', { name: 'Comment' }).click()
  await expect(locate.nodeCommentContent(node)).toBeFocused()
  const NEW_COMMENT = 'New comment text'
  await nodeComment.fill(NEW_COMMENT)
  await page.keyboard.press(`Enter`)
  await expect(nodeComment).not.toBeFocused()
  await expect(nodeComment).toHaveText(NEW_COMMENT)
  await expect(locate.nodeCommentContent(page)).toHaveCount(INITIAL_NODE_COMMENTS + 1)
})

test('Delete comment by clearing text', async ({ editorPage, page }) => {
  await editorPage
  const nodeComment = locate.nodeCommentContent(locate.graphNodeByBinding(page, 'final'))
  await expect(nodeComment).toHaveText('This node can be entered')

  await nodeComment.click()
  await page.keyboard.press(`ControlOrMeta+A`)
  await page.keyboard.press(`Delete`)
  await page.keyboard.press(`Enter`)
  await expect(nodeComment).toBeHidden()
})

test('URL added to comment is rendered as link', async ({ editorPage, page, context }) => {
  await editorPage
  const commentContent = locate.nodeCommentContent(locate.graphNodeByBinding(page, 'final'))
  await expect(commentContent).toHaveText('This node can be entered')
  await expect(commentContent.locator('a')).toBeHidden()

  await commentContent.click()
  await page.keyboard.press(`ControlOrMeta+A`)
  const NEW_COMMENT = "Here's a URL: https://example.com"
  await commentContent.fill(NEW_COMMENT)
  await page.keyboard.press(`Enter`)
  await expect(commentContent).not.toBeFocused()
  await expect(commentContent).toHaveText(NEW_COMMENT)
  await expect(commentContent.locator('a')).toHaveCount(1)

  await expect(commentContent.locator('a')).toHaveAccessibleDescription(
    /Click to edit.*Click to open link/,
  )
  await commentContent.locator('a').click()
  await expect(commentContent).toBeFocused()
  await expect(page.locator('.LinkEditPopup')).toBeVisible()
  await page.keyboard.press(`Enter`)
  await expect(commentContent).not.toBeFocused()
  await expect(page.locator('.LinkEditPopup')).toBeHidden()
  context.route('https://example.com', (route) => route.fulfill({ status: 200, body: 'YAY' }))
  const newPagePromise = context.waitForEvent('page', { timeout: 10000 })
  await commentContent.locator('a').click({ modifiers: ['ControlOrMeta'] })
  await expect(newPagePromise).resolves.toHaveURL('https://example.com')
})

test('Long comment displays wrapped', async ({ editorPage, page }) => {
  await editorPage
  const nodeComment = locate.nodeCommentContent(locate.graphNodeByBinding(page, 'final'))
  const shortContentHeight = (await nodeComment.boundingBox())!.height
  await expect(nodeComment).toHaveText('This node can be entered')
  await nodeComment.click()
  await page.keyboard.press(`ControlOrMeta+A`)
  const NEW_COMMENT = 'long comment '.repeat(30)
  await nodeComment.fill(NEW_COMMENT)
  await page.keyboard.press(`Enter`)
  await expect(nodeComment).not.toBeFocused()
  await expect(nodeComment).toHaveText(NEW_COMMENT)
  const longContentHeight = (await nodeComment.boundingBox())!.height
  expect(longContentHeight).toBeGreaterThan(shortContentHeight)
})
