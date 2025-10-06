import type { Page } from 'playwright/test'

/** Adds an init script to the page that sets up a mock clipboard. */
export async function addMockClipboardInitScript(page: Page): Promise<void> {
  await page.addInitScript(() => {
    function useMockClipboard() {
      let contents: ClipboardItem[] = []
      return {
        read: async (): Promise<ClipboardItem[]> => {
          return [...contents]
        },
        write: async (items: ClipboardItem[]) => {
          contents = [...items]
        },
        readText: async (): Promise<string> => {
          for (const item of contents) {
            if (item.types.includes('text/plain')) {
              const blob = await item.getType('text/plain')
              return blob.text()
            }
          }
          return ''
        },
        writeText: async (data: string): Promise<void> => {
          contents = [new ClipboardItem({ 'text/plain': data })]
        },
      }
    }
    Object.assign(window.navigator.clipboard, useMockClipboard())
  })
}
