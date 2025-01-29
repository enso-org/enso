import { type TextMatchTransformer } from '@lexical/markdown'
import {
  type DOMConversionMap,
  type DOMConversionOutput,
  type NodeKey,
  $applyNodeReplacement,
  DecoratorNode,
} from 'lexical'

interface ImagePayload {
  altText: string
  key?: NodeKey | undefined
  src: string
}

function $convertImageElement(domNode: Node): null | DOMConversionOutput {
  if (domNode instanceof HTMLImageElement) {
    const { alt: altText, src } = domNode
    const node = $createImageNode({ altText, src })
    return { node }
  }
  return null
}

function notImplemented(): never {
  throw new Error('not implemented')
}

/** TODO: Add docs */
export class ImageNode extends DecoratorNode<void> {
  __src: string
  __altText: string

  /** TODO: Add docs */
  static override getType(): string {
    return 'image'
  }

  /** TODO: Add docs */
  static override clone(node: ImageNode): ImageNode {
    return new ImageNode(node.__src, node.__altText, node.__key)
  }

  /** TODO: Add docs */
  static override importDOM(): DOMConversionMap | null {
    return {
      img: (_node: Node) => ({
        conversion: $convertImageElement,
        priority: 0,
      }),
    }
  }

  /** TODO: Add docs */
  constructor(src: string, altText: string, key?: NodeKey) {
    super(key)
    this.__src = src
    this.__altText = altText
  }

  /** TODO: Add docs */
  override exportJSON() {
    return {
      altText: this.getAltText(),
      src: this.getSrc(),
      type: 'image',
      version: 1,
    }
  }

  /** TODO: Add docs */
  getSrc(): string {
    return this.__src
  }

  /** TODO: Add docs */
  getAltText(): string {
    return this.__altText
  }

  /** Not used, but required by API */
  override decorate() {}

  static override importJSON = notImplemented
  override exportDOM = notImplemented
  override createDOM = notImplemented
  override updateDOM = notImplemented
}

/** Type predicate for {@link ImageNode} */
export function $isImageNode(node: unknown): node is ImageNode {
  return node instanceof ImageNode
}

function $createImageNode({ altText, src, key }: ImagePayload): ImageNode {
  return $applyNodeReplacement(new ImageNode(src, altText, key))
}

export const IMAGE: TextMatchTransformer = {
  dependencies: [ImageNode],
  export: (node) => {
    if (!$isImageNode(node)) return null
    return `![${node.getAltText()}](${node.getSrc()})`
  },
  regExp: /$^/,
  type: 'text-match',
}
