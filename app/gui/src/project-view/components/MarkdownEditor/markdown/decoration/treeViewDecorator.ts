import { type VueHost } from '@/components/VueHostRender.vue'
import { getVueHost } from '@/util/codemirror/vueHostExt'
import { syntaxTree } from '@codemirror/language'
import { type EditorState, RangeSetBuilder, type Text } from '@codemirror/state'
import {
  Decoration,
  type DecorationSet,
  type EditorView,
  type PluginValue,
  type ViewUpdate,
} from '@codemirror/view'
import { type SyntaxNodeRef, type Tree } from '@lezer/common'

/** Maintains a set of decorations based on the tree, lazily-constructed for the visible range of the document. */
export class TreeViewDecorator implements PluginValue {
  decorations: DecorationSet

  /** Constructor. */
  constructor(
    view: EditorView,
    /**
     * Functions that construct decorations based on a syntax tree. The decorations must not have significant impact on
     * the height of the document, or scrolling issues would result, because decorations are lazily computed based on
     * the current viewport.
     */
    private readonly nodeDecorators: NodeStateDecorator[],
    private readonly isInvalidatedBy: (update: ViewUpdate) => boolean,
  ) {
    this.decorations = this.buildDeco(syntaxTree(view.state), view)
  }

  /** Applies the view update to the decoration set. */
  update(update: ViewUpdate) {
    if (!this.isInvalidatedBy(update)) return
    this.decorations = this.buildDeco(syntaxTree(update.state), update.view)
  }

  private buildDeco(tree: Tree, view: EditorView) {
    if (!tree.length) return Decoration.none
    const vueHost = view.state.facet(getVueHost)
    if (!vueHost) return Decoration.none
    const builder = new RangeSetBuilder<Decoration>()
    const doc = view.state.doc
    const emit = (from: number, to: number, value: Decoration) => {
      builder.add(from, to, value)
    }
    for (const { from, to } of view.visibleRanges) {
      tree.iterate({
        from,
        to,
        enter: (nodeRef) => {
          for (const decorator of this.nodeDecorators)
            decorator(nodeRef, doc, emit, vueHost, view.state)
        },
      })
    }
    return builder.finish()
  }
}

/** Function that applies decorations based on the document text, its parsed syntax, and the editor state. */
export interface NodeStateDecorator {
  (
    nodeRef: SyntaxNodeRef,
    doc: Text,
    emitDecoration: (from: number, to: number, deco: Decoration) => void,
    vueHost: VueHost,
    state: EditorState,
  ): void
}
