import {
  $createImageNode,
  $isImageNode,
  ImageNode,
} from '@/components/MarkdownEditor/ImagePlugin/imageNode'
import type { LexicalMarkdownPlugin } from '@/components/MarkdownEditor/markdown'
import type { TextMatchTransformer } from '@lexical/markdown'
import type { LexicalEditor } from 'lexical'
import { assertDefined } from 'ydoc-shared/util/assert'

export const IMAGE: TextMatchTransformer = {
  dependencies: [ImageNode],
  export: (node) => {
    if (!$isImageNode(node)) {
      return null
    }
    return `![${node.getAltText()}](${node.getSrc()})`
  },
  importRegExp: /!\[([^\]]*)]\(([^()\n]+)\)/,
  regExp: /!\[([^\]]*)]\(([^()\n]+)\)$/,
  replace: (textNode, match) => {
    const [, altText, src] = match
    assertDefined(altText)
    assertDefined(src)
    const imageNode = $createImageNode({ altText, src })
    textNode.replace(imageNode)
  },
  trigger: ')',
  type: 'text-match',
}

export const imagePlugin: LexicalMarkdownPlugin = {
  nodes: [ImageNode],
  transformers: [IMAGE],
  register(_editor: LexicalEditor): void {},
}
