import { IMAGE } from '@/components/MarkdownEditor/htmlToMarkdown/imageNode'
import { LINK } from '@/components/MarkdownEditor/htmlToMarkdown/linkNode'
import { $generateNodesFromDOM } from '@lexical/html'
import { $convertToMarkdownString, TRANSFORMERS } from '@lexical/markdown'
import { $insertNodes, createEditor } from 'lexical'

const domParser = new DOMParser()

const transformers = [IMAGE, LINK, ...TRANSFORMERS]

/** Convert the given HTML text to a Markdown approximation. */
export function htmlToMarkdownImpl(html: string): string {
  const dom = domParser.parseFromString(html, 'text/html')
  const editor = createEditor({
    nodes: transformers.flatMap((transformer) =>
      'dependencies' in transformer ? transformer.dependencies : [],
    ),
  })
  editor.update(() => $insertNodes($generateNodesFromDOM(editor, dom)))
  return editor.read(() => $convertToMarkdownString(transformers))
}
