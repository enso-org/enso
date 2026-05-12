/** @file Renderer-side queue, placeholder, and cancellation behaviour for in-flight AI prompts. */
import { expect, test, type Page } from 'integration-test/base'
import * as locate from './locate'

test.use({ aiAvailable: true })

const aiPendingNode = (page: Page) => page.locator('.AiPendingNode')
const aiPendingStatus = (node: ReturnType<typeof aiPendingNode>) =>
  node.locator('[data-testid="ai-pending-status"]')

/** Switch the AI mock into deferred mode so each `generateComponent` waits for the test to drive it. */
async function enableDeferredAiMock(page: Page): Promise<void> {
  await page.evaluate(() => {
    const w = window as unknown as { __aiMockController: { deferred: boolean } }
    w.__aiMockController.deferred = true
  })
}

interface PendingProbe {
  ids: string[]
  cancels: string[]
}

/** Snapshot the test-side controller state (pending request ids, cancel calls so far). */
async function probeAiMock(page: Page): Promise<PendingProbe> {
  return await page.evaluate(() => {
    const w = window as unknown as {
      __aiMockController: {
        pending: Map<string, unknown>
        cancels: string[]
      }
    }
    const c = w.__aiMockController
    return { ids: Array.from(c.pending.keys()), cancels: c.cancels.slice() }
  })
}

/** Resolve the deferred `generateComponent` for a specific request id with a successful body. */
async function resolveAi(page: Page, requestId: string): Promise<void> {
  await page.evaluate((id) => {
    const w = window as unknown as {
      __aiMockController: {
        pending: Map<string, (reply: { result: { ok: true; value: unknown }; usage: null }) => void>
      }
    }
    const resolve = w.__aiMockController.pending.get(id)
    if (resolve == null) throw new Error(`No pending AI request with id ${id}`)
    w.__aiMockController.pending.delete(id)
    resolve({
      result: {
        ok: true,
        value: {
          functionName: 'ai_helper',
          argumentNames: [],
          body: 'result = 42\nresult',
          callArguments: [],
        },
      },
      usage: null,
    })
  }, requestId)
}

/** Resolve a pending `generateComponent` with a non-cancellation failure. */
async function failAi(page: Page, requestId: string, message: string): Promise<void> {
  await page.evaluate(
    ({ id, msg }) => {
      const w = window as unknown as {
        __aiMockController: {
          pending: Map<
            string,
            (reply: {
              result: { ok: false; error: { payload: string; context: never[] } }
              usage: null
            }) => void
          >
        }
      }
      const resolve = w.__aiMockController.pending.get(id)
      if (resolve == null) throw new Error(`No pending AI request with id ${id}`)
      w.__aiMockController.pending.delete(id)
      resolve({
        result: { ok: false, error: { payload: msg, context: [] } },
        usage: null,
      })
    },
    { id: requestId, msg: message },
  )
}

/** Fire an `aiProgress` event into every registered renderer-side handler. */
async function emitProgress(page: Page, event: Record<string, unknown>): Promise<void> {
  await page.evaluate((ev) => {
    const w = window as unknown as {
      __aiMockController: { progressHandlers: Array<(event: unknown) => void> }
    }
    for (const h of w.__aiMockController.progressHandlers) h(ev)
  }, event)
}

async function openAiPrompt(page: Page, prompt: string): Promise<void> {
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await page.keyboard.insertText(prompt)
  await expect(page.locator('.ComponentList')).toBeHidden()
  await page.keyboard.press('Enter')
  // Component browser closes immediately on AI submit; the placeholder takes over.
  await expect(locate.componentBrowser(page)).toBeHidden()
}

test('AI placeholder appears, narrates progress, and clears once the agent replies', async ({
  editorPage,
  page,
}) => {
  await editorPage
  await enableDeferredAiMock(page)

  await openAiPrompt(page, 'first prompt')
  const placeholder = aiPendingNode(page)
  await expect(placeholder).toHaveCount(1)
  // The placeholder starts in the waiting state; the main process drives the transition to
  // "Thinking…" via the `started` progress event.
  await expect(aiPendingStatus(placeholder)).toHaveText('Waiting…')

  const probe = await probeAiMock(page)
  expect(probe.ids).toHaveLength(1)
  const requestId = probe.ids[0]!
  await emitProgress(page, { requestId, kind: 'started' })
  await expect(aiPendingStatus(placeholder)).toHaveText('Thinking…')

  // Drive a `text` progress event — placeholder narration updates.
  await emitProgress(page, { requestId, kind: 'text', text: 'Inspecting columns' })
  await expect(aiPendingStatus(placeholder)).toHaveText('Inspecting columns')

  // Resolve successfully — placeholder disappears, the AI node lands.
  await resolveAi(page, requestId)
  await expect(placeholder).toHaveCount(0)
  await expect(locate.graphNodeByBinding(page, 'ai_component1')).toBeVisible()
})

test('queueing two AI prompts shows both placeholders, second is queued', async ({
  editorPage,
  page,
}) => {
  await editorPage
  await enableDeferredAiMock(page)

  await openAiPrompt(page, 'first')
  await openAiPrompt(page, 'second')

  const placeholders = aiPendingNode(page)
  await expect(placeholders).toHaveCount(2)
  // Both entries are user-visibly "queued" until the main process emits `started` on the first.
  // The second carries the renderer-side queue position label.
  await expect(aiPendingStatus(placeholders.nth(0))).toHaveText('Waiting…')
  await expect(aiPendingStatus(placeholders.nth(1))).toContainText('#2')

  // Drive `started` on the first; it flips to "Thinking…", the second's label is unchanged.
  const probe = await probeAiMock(page)
  expect(probe.ids).toHaveLength(1)
  await emitProgress(page, { requestId: probe.ids[0]!, kind: 'started' })
  await expect(aiPendingStatus(placeholders.nth(0))).toHaveText('Thinking…')
  await expect(aiPendingStatus(placeholders.nth(1))).toContainText('#2')
})

test('cancelling a queued placeholder removes it without sending a cancel IPC', async ({
  editorPage,
  page,
}) => {
  await editorPage
  await enableDeferredAiMock(page)

  await openAiPrompt(page, 'first')
  await openAiPrompt(page, 'second')

  const placeholders = aiPendingNode(page)
  await expect(placeholders).toHaveCount(2)
  // The queued entry is the second one; click its cancel button.
  await placeholders.nth(1).locator('.cancel').click()
  await expect(placeholders).toHaveCount(1)

  // No cancel IPC went out — queued entries are dropped renderer-side.
  const probe = await probeAiMock(page)
  expect(probe.cancels).toHaveLength(0)
})

test('cancelling a running placeholder sends a cancel IPC and removes it on reply', async ({
  editorPage,
  page,
}) => {
  await editorPage
  await enableDeferredAiMock(page)

  await openAiPrompt(page, 'first')
  const placeholder = aiPendingNode(page)
  await expect(placeholder).toHaveCount(1)
  await placeholder.locator('.cancel').click()

  // The mock's cancel handler resolves the pending generateComponent with a cancellation Err,
  // and the renderer-side dispatcher then removes the placeholder.
  await expect(placeholder).toHaveCount(0)
  const probe = await probeAiMock(page)
  expect(probe.cancels).toHaveLength(1)
})

test('failed placeholder stays until dismissed via the cancel button', async ({
  editorPage,
  page,
}) => {
  await editorPage
  await enableDeferredAiMock(page)

  await openAiPrompt(page, 'first')
  const placeholders = aiPendingNode(page)
  await expect(placeholders).toHaveCount(1)

  const { ids } = await probeAiMock(page)
  const requestId = ids[0]!
  await failAi(page, requestId, 'Mock failure for testing')

  // The placeholder switches to the failed state and parks the error in the bubble.
  const failed = page.locator('.AiPendingNode.failed')
  await expect(failed).toHaveCount(1)
  await expect(aiPendingStatus(failed)).toContainText('Mock failure for testing')

  // Submit a second prompt; the failed placeholder must persist alongside the new pending one.
  // This both rules out any auto-dismiss path (which would drop the count back to 1) and
  // exercises the event-driven renderer cycle so the check isn't a fixed-sleep race.
  await openAiPrompt(page, 'second')
  await expect(placeholders).toHaveCount(2)
  await expect(failed).toHaveCount(1)

  // Dismissing the failed placeholder via its cancel button finally clears it; the second
  // (still-pending) placeholder is unaffected.
  await failed.locator('.cancel').click()
  await expect(failed).toHaveCount(0)
  await expect(placeholders).toHaveCount(1)
})
