import { markdownClipboard } from '@/components/MarkdownEditor/codemirror/clipboard'
import { markdownDecorators } from '@/components/MarkdownEditor/codemirror/decoration'
import { markdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'
import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
export { useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'

interface EnsoMarkdownOptions {
  tryUploadPastedImage: ToValue<((item: ClipboardItem) => boolean) | undefined>
}

/**
 * CodeMirror Extension for the Enso Markdown dialect.
 */
export function ensoMarkdown({ tryUploadPastedImage }: EnsoMarkdownOptions): Extension {
  return [
    ensoMarkdownSyntax(),
    markdownDecorators(),
    markdownFormatting(),
    markdownClipboard({ tryUploadPastedImage }),
  ]
}
