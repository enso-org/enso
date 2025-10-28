import { markdownDecorators } from '@/components/MarkdownEditor/codemirror/decoration'
import { markdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'
import {
  markdownClipboard,
  type MarkdownClipboardOptions,
} from '@/components/MarkdownEditor/codemirror/markdownClipboard'
import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import type { Extension } from '@codemirror/state'
export { useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'

export interface EnsoMarkdownOptions extends MarkdownClipboardOptions {}

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
