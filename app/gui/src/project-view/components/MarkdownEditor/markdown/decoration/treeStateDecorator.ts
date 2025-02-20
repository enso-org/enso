import { type VueHost } from '@/components/VueHostRender.vue'
import { getVueHost, vueHostChanged } from '@/util/codemirror/vueHostExt'
import { syntaxTree } from '@codemirror/language'
import {
  type EditorState,
  type Extension,
  RangeSetBuilder,
  StateField,
  Text,
} from '@codemirror/state'
import { Decoration, DecorationSet, EditorView } from '@codemirror/view'
import type { SyntaxNodeRef, Tree } from '@lezer/common'

/** @returns a CodeMirror extension that maintains a set of decorations based on the syntax tree. */
export function treeStateDecorator(nodeDecorators: NodeDecorator[]): Extension {
  const stateDecorator = new TreeStateDecorator(nodeDecorators)
  function decorate(state: EditorState) {
    const vueHost = state.facet(getVueHost)
    return vueHost ?
        stateDecorator.decorate(syntaxTree(state), state.doc, vueHost)
      : Decoration.none
  }
  return StateField.define<DecorationSet>({
    create(state) {
      return decorate(state)
    },
    update(prev, tr) {
      if (!tr.docChanged && !vueHostChanged(tr)) return prev
      return decorate(tr.state)
    },
    provide: (f) => EditorView.decorations.from(f),
  })
}

class TreeStateDecorator {
  constructor(private readonly nodeDecorators: NodeDecorator[]) {}

  decorate(tree: Tree, doc: Text, vueHost: VueHost): DecorationSet {
    const builder = new RangeSetBuilder<Decoration>()
    const emit = (from: number, to: number, value: Decoration) => {
      builder.add(from, to, value)
    }
    tree.iterate({
      enter: (nodeRef) => {
        for (const decorator of this.nodeDecorators) decorator(nodeRef, doc, emit, vueHost)
      },
    })
    return builder.finish()
  }
}

/** Function that applies decorations based on only the document text and its parsed syntax. */
export interface NodeDecorator {
  (
    nodeRef: SyntaxNodeRef,
    doc: Text,
    emitDecoration: (from: number, to: number, deco: Decoration) => void,
    vueHost: VueHost,
  ): void
}
