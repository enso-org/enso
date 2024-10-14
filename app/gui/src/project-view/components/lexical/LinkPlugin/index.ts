import { documentationEditorBindings } from '@/bindings'
import { IMAGE } from '@/components/MarkdownEditor/ImagePlugin'
import { $isImageNode } from '@/components/MarkdownEditor/ImagePlugin/imageNode'
import type { LexicalMarkdownPlugin } from '@/components/MarkdownEditor/markdown'
import type { LexicalPlugin } from '@/components/lexical'
import { $createLinkNode, $isLinkNode, AutoLinkNode, LinkNode } from '@lexical/link'
import type { Transformer } from '@lexical/markdown'
import { $getNearestNodeOfType } from '@lexical/utils'
import {
  $createTextNode,
  $getSelection,
  $isTextNode,
  CLICK_COMMAND,
  COMMAND_PRIORITY_CRITICAL,
  COMMAND_PRIORITY_LOW,
  SELECTION_CHANGE_COMMAND,
  type LexicalEditor,
} from 'lexical'
import { shallowRef } from 'vue'
import { createLinkMatcherWithRegExp, useAutoLink } from './autoMatcher'

const URL_REGEX =
  /(?<!\]\()((https?:\/\/(www\.)?)|(www\.))[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_+.~#?&//=]*)/

const EMAIL_REGEX =
  /(?<!\]\()(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))/

export const __TEST = { URL_REGEX, EMAIL_REGEX }

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
    } else if (node.getChildrenSize() === 1 && $isImageNode(firstChild)) {
      // Images sometimes happen to be inside links (when importing nodes from HTML).
      // The link is not important for us (this type of layout is not supported in markdown),
      // but we want to display the image.
      return IMAGE.export(firstChild, exportChildren, exportFormat)
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

/** TODO: Add docs */
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

const linkClickHandler = documentationEditorBindings.handler({
  openLink() {
    const link = $getSelectedLinkNode()
    if (link instanceof LinkNode) {
      window.open(link.getURL(), '_blank')?.focus()
      return true
    }
    return false
  },
})

const autoLinkClickHandler = documentationEditorBindings.handler({
  openLink() {
    const link = $getSelectedLinkNode()
    if (link instanceof AutoLinkNode) {
      window.open(link.getURL(), '_blank')?.focus()
      return true
    }
    return false
  },
})

export const linkPlugin: LexicalMarkdownPlugin = {
  nodes: [LinkNode],
  transformers: [LINK],
  register(editor: LexicalEditor): void {
    editor.registerCommand(
      CLICK_COMMAND,
      (event) => linkClickHandler(event),
      COMMAND_PRIORITY_CRITICAL,
    )
  },
}

export const autoLinkPlugin: LexicalPlugin = {
  nodes: [AutoLinkNode],
  register(editor: LexicalEditor): void {
    editor.registerCommand(
      CLICK_COMMAND,
      (event) => autoLinkClickHandler(event),
      COMMAND_PRIORITY_CRITICAL,
    )

    useAutoLink(editor, [
      createLinkMatcherWithRegExp(URL_REGEX, (t) => (t.startsWith('http') ? t : `https://${t}`)),
      createLinkMatcherWithRegExp(EMAIL_REGEX, (text) => `mailto:${text}`),
    ])
  },
}

/** TODO: Add docs */
export function useLinkNode(editor: LexicalEditor) {
  const urlUnderCursor = shallowRef<string>()
  editor.registerCommand(
    SELECTION_CHANGE_COMMAND,
    () => {
      urlUnderCursor.value = $getSelectedLinkNode()?.getURL()
      return false
    },
    COMMAND_PRIORITY_LOW,
  )
  return { urlUnderCursor }
}
