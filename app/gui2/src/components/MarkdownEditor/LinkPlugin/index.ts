import type { LexicalMarkdownPlugin } from '@/components/MarkdownEditor/markdown'
import {
  $createAutoLinkNode,
  $createLinkNode,
  $isAutoLinkNode,
  $isLinkNode,
  AutoLinkNode,
  LinkNode,
} from '@lexical/link'
import { type Transformer } from '@lexical/markdown'
import { $getNearestNodeOfType } from '@lexical/utils'
import {
  $createTextNode,
  $getSelection,
  $isTextNode,
  CLICK_COMMAND,
  COMMAND_PRIORITY_CRITICAL,
  type LexicalEditor,
} from 'lexical'

const _AUTO_LINK: Transformer = {
  dependencies: [AutoLinkNode],
  export: (node, exportChildren, exportFormat) => {
    if (!$isAutoLinkNode(node)) {
      return null
    }
    const title = node.getTitle()
    const linkContent =
      title ?
        `[${node.getTextContent()}](${node.getURL()} "${title}")`
      : `[${node.getTextContent()}](${node.getURL()})`
    const firstChild = node.getFirstChild()
    // Add text styles only if link has single text node inside. If it's more
    // then one we ignore it as markdown does not support nested styles for links
    if (node.getChildrenSize() === 1 && $isTextNode(firstChild)) {
      return exportFormat(firstChild, linkContent)
    } else {
      return linkContent
    }
  },
  importRegExp: /(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))/,
  regExp: /(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))$/,
  replace: (textNode, match) => {
    const [, linkText, linkUrl] = match
    if (linkUrl) {
      const linkNode = $createAutoLinkNode(linkUrl)
      const linkTextNode = $createTextNode(linkText)
      linkTextNode.setFormat(textNode.getFormat())
      linkNode.append(linkTextNode)
      textNode.replace(linkNode)
    }
  },
  trigger: ')',
  type: 'text-match',
}

const LINK: Transformer = {
  dependencies: [LinkNode],
  export: (node, exportChildren, exportFormat) => {
    if (!$isLinkNode(node)) {
      return null
    }
    const title = node.getTitle()
    const linkContent =
      title ?
        `[${node.getTextContent()}](${node.getURL()} "${title}")`
      : `[${node.getTextContent()}](${node.getURL()})`
    const firstChild = node.getFirstChild()
    // Add text styles only if link has single text node inside. If it's more
    // then one we ignore it as markdown does not support nested styles for links
    if (node.getChildrenSize() === 1 && $isTextNode(firstChild)) {
      return exportFormat(firstChild, linkContent)
    } else {
      return linkContent
    }
  },
  importRegExp: /(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))/,
  regExp: /(?:\[([^[]+)\])(?:\((?:([^()\s]+)(?:\s"((?:[^"]*\\")*[^"]*)"\s*)?)\))$/,
  replace: (textNode, match) => {
    const [, linkText, linkUrl, linkTitle] = match
    if (linkText && linkUrl) {
      const linkNode = $createLinkNode(linkUrl, {
        title: linkTitle ?? null,
        rel: 'nofollow',
        target: '_blank',
      })
      const linkTextNode = $createTextNode(linkText)
      linkTextNode.setFormat(textNode.getFormat())
      linkNode.append(linkTextNode)
      textNode.replace(linkNode)
    }
  },
  trigger: ')',
  type: 'text-match',
}

export function $getSelectedLinkNode() {
  const selection = $getSelection()
  if (selection?.isCollapsed) {
    const node = selection?.getNodes()[0]
    if (node) {
      return (
        $getNearestNodeOfType(node, LinkNode) ??
        $getNearestNodeOfType(node, AutoLinkNode) ??
        undefined
      )
    }
  }
}

export const linkPlugin: LexicalMarkdownPlugin = {
  nodes: [LinkNode, AutoLinkNode],
  transformers: [LINK],
  register(editor: LexicalEditor): void {
    editor.registerCommand(
      CLICK_COMMAND,
      (event) => {
        if (event.ctrlKey || event.metaKey) {
          const link = $getSelectedLinkNode()
          if (link) {
            window.open(link.getURL(), '_blank')?.focus()
            return true
          }
        }
        return false
      },
      COMMAND_PRIORITY_CRITICAL,
    )
  },
}
