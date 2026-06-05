/**
 * @file Editing an existing AI node: the CB opens in AI mode locked to the existing prompt,
 * the original node is hidden behind a placeholder while the request is in flight, and a
 * successful reply rewrites the node in place (preserving the binding identifier).
 */
import { expect, test, type Page } from 'integration-test/base'
import * as locate from './locate'

test.use({ aiAvailable: true })

interface MockController {
  deferred: boolean
  requests: Array<{
    requestId: string
    prompt: string
    context: { editContext?: { previousPrompt: string; previousDefinition?: string } }
  }>
  pending: Map<string, (reply: unknown) => void>
}

async function enableDeferredAiMock(page: Page): Promise<void> {
  await page.evaluate(() => {
    const w = window as unknown as { __aiMockController: MockController }
    w.__aiMockController.deferred = true
  })
}

async function resolveLastRequest(
  page: Page,
  reply: { functionName: string; argumentNames: string[]; body: string; callArguments: string[] },
): Promise<void> {
  await page.evaluate((replyValue) => {
    const w = window as unknown as { __aiMockController: MockController }
    const last = w.__aiMockController.requests.at(-1)
    if (!last) throw new Error('No pending AI requests to resolve.')
    const resolve = w.__aiMockController.pending.get(last.requestId)
    if (!resolve) throw new Error('Last request is not deferred.')
    w.__aiMockController.pending.delete(last.requestId)
    resolve({ result: { ok: true, value: replyValue }, usage: null })
  }, reply)
}

async function readLastRequest(page: Page): Promise<{
  prompt: string
  editContext?: { previousPrompt: string; previousDefinition?: string }
}> {
  return await page.evaluate(() => {
    const w = window as unknown as { __aiMockController: MockController }
    const last = w.__aiMockController.requests.at(-1)
    if (!last) throw new Error('No requests captured.')
    return { prompt: last.prompt, editContext: last.context.editContext }
  })
}

test('Editing an existing AI node sends prior prompt + definition and updates the node in place', async ({
  editorPage,
  page,
}) => {
  await editorPage

  // 1. Create an AI node (immediate-reply mock yields functionName=ai_helper).
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await page.keyboard.insertText('first prompt')
  await page.keyboard.press('Enter')
  const aiNode = locate.graphNodeByBinding(page, 'ai_component1')
  await expect(aiNode).toBeVisible()
  await expect(aiNode.locator('.WidgetAiPrompt')).toHaveText('first prompt')

  // 2. Re-open the CB on that node via Mod+Click (the `nodeEditBindings.edit` shortcut).
  await enableDeferredAiMock(page)
  const isMac = process.platform === 'darwin'
  await aiNode.click({ modifiers: [isMac ? 'Meta' : 'Control'] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  // The CB's CodeMirror content is pre-filled with the previous prompt; the icon menu is locked.
  await expect(page.locator('.ModeMenu.locked')).toBeVisible()
  // Replace the prompt — Mod+A then type to overwrite the prefilled text.
  await page.keyboard.press(isMac ? 'Meta+A' : 'Control+A')
  await page.keyboard.type('second prompt')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()

  // 3. While the agent is mid-reply, the original node should be hidden (CSS-hidden via the
  //    `edited` prop wired to `aiPrompts.hiddenNodeIds`).
  await expect(aiNode).toBeHidden()
  // The placeholder takes its place.
  await expect(page.locator('.AiPendingNode')).toHaveCount(1)

  // 4. Verify the agent received the previous prompt + definition.
  const captured = await readLastRequest(page)
  expect(captured.prompt).toBe('second prompt')
  expect(captured.editContext).toBeDefined()
  expect(captured.editContext!.previousPrompt).toBe('first prompt')
  expect(captured.editContext!.previousDefinition).toContain('ai_helper')

  // 5. Resolve the request — the node updates in place, the placeholder disappears, the
  //    binding identifier stays `ai_component1`.
  await resolveLastRequest(page, {
    functionName: 'ai_helper_v2',
    argumentNames: [],
    body: `result = 99\nresult`,
    callArguments: [],
  })
  await expect(page.locator('.AiPendingNode')).toHaveCount(0)
  await expect(aiNode).toBeVisible()
  await expect(aiNode.locator('.WidgetAiPrompt')).toHaveText('second prompt')
})

test('Cancelling an in-flight AI edit restores the original node', async ({ editorPage, page }) => {
  await editorPage

  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await page.keyboard.insertText('initial prompt')
  await page.keyboard.press('Enter')
  const aiNode = locate.graphNodeByBinding(page, 'ai_component1')
  await expect(aiNode).toBeVisible()

  await enableDeferredAiMock(page)
  const isMac = process.platform === 'darwin'
  await aiNode.click({ modifiers: [isMac ? 'Meta' : 'Control'] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  await page.keyboard.press(isMac ? 'Meta+A' : 'Control+A')
  await page.keyboard.type('replacement prompt')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(aiNode).toBeHidden()
  await expect(page.locator('.AiPendingNode')).toHaveCount(1)

  // Cancel via the placeholder's X button.
  await page.locator('.AiPendingNode .cancel').click()
  await expect(page.locator('.AiPendingNode')).toHaveCount(0)
  await expect(aiNode).toBeVisible()
  // The original prompt is unchanged.
  await expect(aiNode.locator('.WidgetAiPrompt')).toHaveText('initial prompt')
})
