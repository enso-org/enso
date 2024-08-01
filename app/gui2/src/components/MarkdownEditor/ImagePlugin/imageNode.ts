import LexicalImage from '@/components/MarkdownEditor/ImagePlugin/LexicalImage.vue'
import type {
  DOMConversionMap,
  DOMConversionOutput,
  DOMExportOutput,
  EditorConfig,
  LexicalNode,
  NodeKey,
  SerializedLexicalNode,
  Spread,
} from 'lexical'
import { $applyNodeReplacement, DecoratorNode } from 'lexical'
import { h, type Component } from 'vue'

export interface ImagePayload {
  altText: string
  key?: NodeKey | undefined
  src: string
}

export interface UpdateImagePayload {
  altText?: string
}

function $convertImageElement(domNode: Node): null | DOMConversionOutput {
  if (domNode instanceof HTMLImageElement) {
    const { alt: altText, src } = domNode
    const node = $createImageNode({ altText, src })
    return { node }
  }
  return null
}

export type SerializedImageNode = Spread<
  {
    altText: string
    src: string
  },
  SerializedLexicalNode
>

export class ImageNode extends DecoratorNode<Component> {
  __src: string
  __altText: string

  static override getType(): string {
    return 'image'
  }

  static override clone(node: ImageNode): ImageNode {
    return new ImageNode(node.__src, node.__altText, node.__key)
  }

  static override importJSON(serializedNode: SerializedImageNode): ImageNode {
    const { altText, src } = serializedNode
    return $createImageNode({
      altText,
      src,
    })
  }

  static override importDOM(): DOMConversionMap | null {
    return {
      img: (_node: Node) => ({
        conversion: $convertImageElement,
        priority: 0,
      }),
    }
  }

  constructor(src: string, altText: string, key?: NodeKey) {
    super(key)
    this.__src = src
    this.__altText = altText
  }

  override exportDOM(): DOMExportOutput {
    const element = document.createElement('img')
    element.setAttribute('src', this.__src)
    element.setAttribute('alt', this.__altText)
    return { element }
  }

  override exportJSON(): SerializedImageNode {
    return {
      altText: this.getAltText(),
      src: this.getSrc(),
      type: 'image',
      version: 1,
    }
  }

  getSrc(): string {
    return this.__src
  }

  getAltText(): string {
    return this.__altText
  }

  setAltText(altText: string): void {
    const writable = this.getWritable()
    writable.__altText = altText
  }

  update(payload: UpdateImagePayload): void {
    const writable = this.getWritable()
    const { altText } = payload
    if (altText !== undefined) {
      writable.__altText = altText
    }
  }

  // View

  override createDOM(config: EditorConfig): HTMLElement {
    const span = document.createElement('span')
    const className = config.theme.image
    if (className !== undefined) {
      span.className = className
    }
    return span
  }

  override updateDOM(_prevNode: ImageNode, dom: HTMLElement, config: EditorConfig): false {
    const className = config.theme.image
    if (className !== undefined) {
      dom.className = className
    }
    return false
  }

  override decorate(): Component {
    return h(LexicalImage, {
      src: this.__src,
      alt: this.__altText,
    })
  }
}

export function $createImageNode({ altText, src, key }: ImagePayload): ImageNode {
  return $applyNodeReplacement(new ImageNode(src, altText, key))
}

export function $isImageNode(node: LexicalNode | null | undefined): node is ImageNode {
  return node instanceof ImageNode
}
