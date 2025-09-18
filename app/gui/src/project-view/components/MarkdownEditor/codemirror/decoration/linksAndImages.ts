import DocumentationImage from '@/components/MarkdownEditor/DocumentationImage.vue'
import DocumentationVideo from '@/components/MarkdownEditor/DocumentationVideo.vue'
import { TreeViewDecorator } from '@/components/MarkdownEditor/codemirror/decoration/treeViewDecorator'
import { VueDecorationWidget } from '@/components/MarkdownEditor/codemirror/decoration/vueDecorationWidget'
import {
  analyzeAutolink,
  analyzeLinkOrImage,
  nodeRange,
} from '@/components/MarkdownEditor/markdown/trees'
import type { VueHost } from '@/components/VueHostRender.vue'
import { linkEditPopup } from '@/util/codemirror/linkEditPopup'
import { linkAttributesFactory, linkAttributesFactoryChanged } from '@/util/codemirror/links'
import { vueHostChanged } from '@/util/codemirror/vueHostExt'
import { Prec, type EditorState, type Extension, type Text } from '@codemirror/state'
import { Decoration, ViewPlugin, WidgetType } from '@codemirror/view'
import type { SyntaxNodeRef } from '@lezer/common'
import { Range } from 'ydoc-shared/util/data/range'

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
  const parsed = analyzeAutolink(nodeRef)
  if (!parsed) return
  const { linkOrImage, text, url, title } = parsed
  return {
    linkOrImage,
    text,
    url: doc.sliceString(url.from, url.to),
    title,
  }
}

function decorateLink(
  nodeRef: SyntaxNodeRef,
  doc: Text,
  emitDecoration: (range: Range, deco: Decoration) => void,
  _vueHost: VueHost,
  state: EditorState,
) {
  const makeAttributes = state.field(linkAttributesFactory)
  if (!makeAttributes) return
  const parsed =
    nodeRef.name === 'Link' ? parseLinkLike(nodeRef, doc)
    : nodeRef.name === 'Autolink' ? parseAutolink(nodeRef, doc)
    : undefined
  if (!parsed) return
  const { linkOrImage: link, text, url, title } = parsed
  if (text.empty) return
  emitDecoration(
    link,
    Decoration.mark({
      tagName: 'span',
      attributes: { 'data-href': url },
    }),
  )
  const attributes = makeAttributes(url)
  emitDecoration(
    text,
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
  emitDecoration: (range: Range, deco: Decoration) => void,
) {
  if (nodeRef.name === 'Image') {
    emitDecoration(
      nodeRange(nodeRef),
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
  emitDecoration: (range: Range, deco: Decoration) => void,
  vueHost: VueHost,
) {
  if (nodeRef.name === 'Image') {
    const parsed = parseLinkLike(nodeRef, doc)
    if (!parsed) return
    const { text, url } = parsed
    const alt = doc.sliceString(text.from, text.to)
    const widget = new MediaWidget({ src: url, alt }, vueHost)
    emitDecoration(
      Range.emptyAt(nodeRef.to),
      Decoration.widget({
        widget,
        // Ensure the cursor is drawn relative to the content before the widget.
        // If it is drawn relative to the widget, it will be hidden when the widget is hidden (i.e. during editing).
        side: 1,
      }),
    )
  }
}

class MediaWidget extends VueDecorationWidget<{ alt: string; src: string }> {
  constructor(props: { alt: string; src: string }, vueHost: VueHost) {
    const isVideo = props.src.match(/https:\/\/www\.youtube(-nocookie)?\.com\/embed\/[^/]+/)
    const component = isVideo ? DocumentationVideo : DocumentationImage
    super(component, props, vueHost, 'cm-media-rendered', 'span')
  }

  override eq(other: WidgetType) {
    return (
      other instanceof MediaWidget &&
      other.props.src == this.props.src &&
      other.props.alt == this.props.alt
    )
  }
}

// === Common ===

interface LinkLikeInfo {
  linkOrImage: Range
  text: Range
  url: string
  title?: string | undefined
}

/** Parse a link or image */
function parseLinkLike(nodeRef: SyntaxNodeRef, doc: Text): LinkLikeInfo | undefined {
  const parsed = analyzeLinkOrImage(nodeRef)
  if (!parsed) return
  const { linkOrImage, text, url, title } = parsed
  return {
    linkOrImage,
    text,
    url: doc.sliceString(url.from, url.to),
    title: title && doc.sliceString(title.from, title.to),
  }
}
