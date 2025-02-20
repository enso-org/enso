import DocumentationImage from '@/components/MarkdownEditor/DocumentationImage.vue'
import { TreeViewDecorator } from '@/components/MarkdownEditor/markdown/decoration/treeViewDecorator'
import { type VueHost } from '@/components/VueHostRender.vue'
import { linkEditPopup } from '@/util/codemirror/linkEditPopup'
import { linkAttributesFactory, linkAttributesFactoryChanged } from '@/util/codemirror/links'
import { vueHostChanged } from '@/util/codemirror/vueHostExt'
import { type EditorState, Extension, Prec, type Text } from '@codemirror/state'
import { Decoration, ViewPlugin, WidgetType } from '@codemirror/view'
import { type SyntaxNodeRef } from '@lezer/common'
import { h, markRaw } from 'vue'
import { isNodeType } from 'ydoc-shared/util/lezer'

// === Links ===

/** Extension applying decorations to Markdown links. */
export function linkDecoratorExt() {
  return Prec.lowest(
    ViewPlugin.define(
      (view) =>
        new TreeViewDecorator(
          view,
          [decorateLink],
          (update) =>
            update.docChanged ||
            update.viewportChanged ||
            linkAttributesFactoryChanged(update) ||
            vueHostChanged(update),
        ),
      {
        decorations: (v) => v.decorations,
      },
    ),
  )
}

/** A CodeMirror extension that displays a popup when the cursor is inside a markdown link. */
export function markdownLinkEditPopup(): Extension {
  return linkEditPopup((el) => (el instanceof HTMLElement ? el.dataset.href : undefined))
}

function parseAutolink(nodeRef: SyntaxNodeRef, doc: Text): LinkLikeInfo | undefined {
  const cursor = nodeRef.node.cursor()
  if (!cursor.firstChild()) return // <
  const linkFrom = cursor.from
  if (!cursor.nextSibling()) return
  if (!isNodeType(cursor, 'URL')) return
  const textFrom = cursor.from
  const textTo = cursor.to
  const url = doc.sliceString(cursor.from, cursor.to)
  let linkTo = cursor.to
  cursor.nextSibling()
  if (isNodeType(cursor, 'LinkMark')) linkTo = cursor.to
  return {
    linkFrom,
    linkTo,
    textFrom,
    textTo,
    url,
  }
}

function decorateLink(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (from: number, to: number, deco: Decoration) => void,
  _vueHost: VueHost,
  state: EditorState,
) {
  const makeAttributes = state.facet(linkAttributesFactory)
  if (!makeAttributes) return
  const parsed =
    nodeRef.name === 'Link' ? parseLinkLike(nodeRef, doc)
    : nodeRef.name === 'Autolink' ? parseAutolink(nodeRef, doc)
    : undefined
  if (!parsed) return
  const { linkFrom, linkTo, textFrom, textTo, url, title } = parsed
  if (textFrom === textTo) return
  emitDecoration(
    linkFrom,
    linkTo,
    Decoration.mark({
      tagName: 'span',
      attributes: { 'data-href': url },
    }),
  )
  const attributes = makeAttributes(url)
  emitDecoration(
    textFrom,
    textTo,
    Decoration.mark({
      tagName: 'a',
      attributes: title ? { title, ...attributes } : attributes,
    }),
  )
}

// === Images ===

/** Extension applying a CSS to image markup. */
export function decorateImageWithClass(
  nodeRef: SyntaxNodeRef,
  _doc: Text,
  emitDecoration: (from: number, to: number, deco: Decoration) => void,
) {
  if (nodeRef.name === 'Image') {
    emitDecoration(
      nodeRef.from,
      nodeRef.to,
      Decoration.mark({
        class: 'cm-image-markup',
      }),
    )
  }
}

/** Extension creating a widget that renders an image. */
export function decorateImageWithRendered(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (from: number, to: number, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name === 'Image') {
    const parsed = parseLinkLike(nodeRef, doc)
    if (!parsed) return
    const { textFrom, textTo, url } = parsed
    const text = doc.sliceString(textFrom, textTo)
    const widget = new ImageWidget({ alt: text, src: url }, vueHost)
    emitDecoration(
      nodeRef.to,
      nodeRef.to,
      Decoration.widget({
        widget,
        // Ensure the cursor is drawn relative to the content before the widget.
        // If it is drawn relative to the widget, it will be hidden when the widget is hidden (i.e. during editing).
        side: 1,
      }),
    )
  }
}

class ImageWidget extends WidgetType {
  private container: HTMLElement | undefined
  private vueHostRegistration: { unregister: () => void } | undefined

  constructor(
    private readonly props: {
      readonly alt: string
      readonly src: string
    },
    private readonly vueHost: VueHost,
  ) {
    super()
  }

  override get estimatedHeight() {
    return -1
  }

  override eq(other: WidgetType) {
    return (
      other instanceof ImageWidget &&
      other.props.src == this.props.src &&
      other.props.alt == this.props.alt
    )
  }

  override toDOM(): HTMLElement {
    if (!this.container) {
      const container = markRaw(document.createElement('span'))
      container.className = 'cm-image-rendered'
      this.vueHostRegistration = this.vueHost.register(
        h(DocumentationImage, {
          src: this.props.src,
          alt: this.props.alt,
        }),
        container,
      )
      this.container = container
    }
    return this.container
  }

  override destroy() {
    this.vueHostRegistration?.unregister()
    this.container = undefined
  }
}

// === Common ===

interface LinkLikeInfo {
  linkFrom: number
  linkTo: number
  textFrom: number
  textTo: number
  url: string
  title?: string | undefined
}

/** Parse a link or image */
function parseLinkLike(nodeRef: SyntaxNodeRef, doc: Text): LinkLikeInfo | undefined {
  const cursor = nodeRef.node.cursor()
  if (!cursor.firstChild()) return
  const linkFrom = cursor.from // [ or ![
  const textFrom = cursor.to
  if (!cursor.nextSibling()) return
  const textTo = cursor.from // ]
  do {
    if (!cursor.nextSibling()) return
  } while (!isNodeType(cursor, 'URL'))
  const url = doc.sliceString(cursor.from, cursor.to)
  cursor.nextSibling()
  let title: string | undefined = undefined
  if (isNodeType(cursor, 'LinkTitle')) {
    title = doc.sliceString(cursor.from, cursor.to)
    cursor.nextSibling()
  }
  let linkTo: number
  do {
    linkTo = cursor.to
    if (!cursor.nextSibling()) break
  } while (isNodeType(cursor, 'LinkMark'))
  return {
    linkFrom,
    linkTo,
    textFrom,
    textTo,
    url,
    title,
  }
}
