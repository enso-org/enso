import { textEditorsCommonBindings } from '@/bindings'
import { htmlToMarkdown } from '@/components/MarkdownEditor/htmlToMarkdown'
import { putText } from '@/util/codemirror'
import { handlerToKeyBinding, type CmEvent } from '@/util/codemirror/keymap'
import { LINKABLE_URL_REGEX } from '@/util/link'
import type { Extension } from '@codemirror/state'
import { EditorView, keymap } from '@codemirror/view'
import { prerenderMarkdown } from 'ydoc-shared/ast/documentation'

function uriEscapeChar(char: string) {
  return `%${char.codePointAt(0)!.toString(16).toUpperCase().padStart(2, '0')}`
}

function toAutoLink(text: string) {
  return `<${text.replaceAll(/[\][<>*`]/g, uriEscapeChar)}>`
}

/** Convert the input to Markdown. This includes converting any likely URLs to <autolink>s. */
export function transformPastedText(text: string): string {
  return text.replaceAll(LINKABLE_URL_REGEX, toAutoLink)
}

export interface MarkdownClipboardOptions {
  customClipboardAction: (item: ClipboardItem) => boolean
  customDropAction: (event: DragEvent) => boolean
}

/** @returns a CodeMirror extension customizing the clipboard for Enso Markdown. */
export function markdownClipboard({
  customClipboardAction,
  customDropAction,
}: MarkdownClipboardOptions): Extension {
  function handlePaste(event: CmEvent, raw: boolean) {
    const view = event.codemirrorView
    window.navigator.clipboard.read().then((items) => handleClipboardItems(view, items, raw))
  }
  async function handleClipboardItems(view: EditorView, items: ClipboardItem[], raw: boolean) {
    for (const item of items) {
      if (customClipboardAction?.(item)) continue
      const htmlType = item.types.find((type) => type === 'text/html')
      if (htmlType) {
        const blob = await item.getType(htmlType)
        const html = await blob.text()
        const markdown = prerenderMarkdown(await htmlToMarkdown(html))
        putText(view, markdown)
        continue
      }
      const textType = item.types.find((type) => type === 'text/plain')
      if (textType) {
        const blob = await item.getType(textType)
        const rawText = await blob.text()
        putText(view, raw ? rawText : transformPastedText(rawText))
      }
    }
  }

  return [
    EditorView.clipboardInputFilter.of(transformPastedText),
    EditorView.domEventHandlers({ drop: customDropAction }),
    keymap.of([
      handlerToKeyBinding(
        textEditorsCommonBindings.handler({
          'textEditor.paste': (event) => handlePaste(event, false),
          'textEditor.pasteRaw': (event) => handlePaste(event, true),
        }),
        true,
      ),
    ]),
  ]
}
