import { $isImageNode, IMAGE } from '@/components/MarkdownEditor/htmlToMarkdown/imageNode'
import { $isLinkNode, LinkNode } from '@lexical/link'
import { type Transformer } from '@lexical/markdown'
import { $isTextNode } from 'lexical'
export { LinkNode } from '@lexical/link'

export const LINK: Transformer = {
  dependencies: [LinkNode],
  export: (node, exportChildren, exportFormat) => {
    if (!$isLinkNode(node)) return null
    const title = node.getTitle()
    const linkContent =
      title ?
        `[${node.getTextContent()}](${node.getURL()} "${title}")`
      : `[${node.getTextContent()}](${node.getURL()})`
    const firstChild = node.getFirstChild()
    // Add text styles only if link has single text node inside. If it's more
    // than one we ignore it as markdown does not support nested styles for links
    if (node.getChildrenSize() === 1 && $isTextNode(firstChild)) {
      return exportFormat(firstChild, linkContent)
    } else if (node.getChildrenSize() === 1 && $isImageNode(firstChild)) {
      // Images sometimes happen to be inside links (when importing nodes from HTML).
      // The link is not important for us (this type of layout is not supported in markdown),
      // but we want to display the image.
      return IMAGE.export!(firstChild, exportChildren, exportFormat)
    } else {
      return linkContent
    }
  },
  regExp: /$^/,
  type: 'text-match',
}
