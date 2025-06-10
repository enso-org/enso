import { markdownDecorators } from '@/components/MarkdownEditor/codemirror/decoration'
import { markdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'
import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import { type Extension } from '@codemirror/state'
export { useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror/formatting'

/**
 * CodeMirror Extension for the Enso Markdown dialect.
 */
export function ensoMarkdown(): Extension {
  return [ensoMarkdownSyntax(), markdownDecorators(), markdownFormatting()]
}
