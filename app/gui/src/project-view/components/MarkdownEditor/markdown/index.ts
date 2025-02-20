import { markdownDecorators } from '@/components/MarkdownEditor/markdown/decoration'
import { ensoMarkdownSyntax } from '@/components/MarkdownEditor/markdown/syntax'
import { type Extension } from '@codemirror/state'

/**
 * CodeMirror Extension for the Enso Markdown dialect.
 */
export function ensoMarkdown(): Extension {
  return [ensoMarkdownSyntax(), markdownDecorators()]
}
