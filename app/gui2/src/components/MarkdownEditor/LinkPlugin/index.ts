import type { LexicalMarkdownPlugin } from '@/components/MarkdownEditor/markdown'
import { $createLinkNode, $isLinkNode, AutoLinkNode, LinkNode } from '@lexical/link'
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
import { createLinkMatcherWithRegExp, useAutoLink } from './autoMatcher'

const URL_REGEX =
  /((https?:\/\/(www\.)?)|(www\.))[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_+.~#?&//=]*)/

const EMAIL_REGEX =
  /(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))/

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

    useAutoLink(editor, [
      createLinkMatcherWithRegExp(URL_REGEX, (t) => (t.startsWith('http') ? t : `https://${t}`)),
      createLinkMatcherWithRegExp(EMAIL_REGEX, (text) => `mailto:${text}`),
    ])
  },
}
