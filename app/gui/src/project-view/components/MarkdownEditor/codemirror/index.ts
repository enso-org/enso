import {
  markdownClipboard,
  type MarkdownClipboardOptions,
} from '@/components/MarkdownEditor/codemirror/clipboard'
import { markdownDecorators } from '@/components/MarkdownEditor/codemirror/decoration'
import { markdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'
import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import type { Extension } from '@codemirror/state'
export { useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'

interface EnsoMarkdownOptions extends MarkdownClipboardOptions {
  tryUploadPastedImage: (item: ClipboardItem) => boolean
  tryUploadDroppedImage: (event: DragEvent) => boolean
}

/**
 * CodeMirror Extension for the Enso Markdown dialect.
 */
export function ensoMarkdown(options: EnsoMarkdownOptions): Extension {
  return [
    ensoMarkdownSyntax(),
    markdownDecorators(),
    markdownFormatting(),
    markdownClipboard(options),
  ]
}
