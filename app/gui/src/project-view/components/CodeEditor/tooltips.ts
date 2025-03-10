import CodeEditorTooltip from '@/components/CodeEditor/CodeEditorTooltip.vue'
import { astProp } from '@/components/CodeEditor/ensoSyntax'
import { type VueHost } from '@/components/VueHostRender.vue'
import { type GraphStore } from '@/stores/graph'
import { type SuggestionDbStore } from '@/stores/suggestionDatabase'
import { Ast } from '@/util/ast'
import { type ToValue } from '@/util/reactivity'
import { syntaxTree } from '@codemirror/language'
import { type Extension } from '@codemirror/state'
import {
  type EditorView,
  hoverTooltip as originalHoverTooltip,
  tooltips,
  type TooltipView,
} from '@codemirror/view'
import { type SyntaxNode } from '@lezer/common'
import * as iter from 'enso-common/src/utilities/data/iter'
import { h, markRaw, toValue } from 'vue'
import { syntaxNodeAncestors } from 'ydoc-shared/util/lezer'

/** TODO: Add docs */
function hoverTooltip(
  create: (
    syntax: SyntaxNode,
  ) => TooltipView | ((view: EditorView) => TooltipView) | null | undefined,
): Extension {
  return [
    tooltips({ position: 'absolute' }),
    originalHoverTooltip((view, pos, side) => {
      const syntaxNode = syntaxTree(view.state).resolveInner(pos, side)
      const domOrCreate = create(syntaxNode)
      if (domOrCreate == null) return null

      return {
        pos: syntaxNode.from,
        end: syntaxNode.to,
        above: true,
        arrow: true,
        create: typeof domOrCreate !== 'function' ? () => domOrCreate : domOrCreate,
      }
    }),
  ]
}

function codeEditorTooltip(vueHost: VueHost, props: typeof CodeEditorTooltip.props): TooltipView {
  const dom = markRaw(document.createElement('div'))
  dom.classList.add('CodeEditorTooltip')
  const vueHostRegistration = vueHost.register(h(CodeEditorTooltip, props), dom)
  return { dom, destroy: vueHostRegistration.unregister }
}

/** @returns A CodeMirror extension that creates tooltips containing type and syntax information for Enso code. */
export function ensoHoverTooltip(
  graphStore: Pick<GraphStore, 'moduleSource' | 'db'>,
  suggestionDbStore: SuggestionDbStore,
  vueHost: ToValue<VueHost | undefined>,
) {
  return hoverTooltip((syn) => {
    const vueHostValue = toValue(vueHost)
    if (!vueHostValue) {
      console.error('Cannot render tooltip without Vue host.')
      return
    }
    const enclosingAstNodes = iter.map(syntaxNodeAncestors(syn), (syn) => syn.tree?.prop(astProp))
    const enclosingAsts = iter.filter(enclosingAstNodes, (node) => node instanceof Ast.Ast)
    const enclosingExternalIds = iter.map(enclosingAsts, ({ externalId }) => externalId)
    const nodeId = iter.find(enclosingExternalIds, graphStore.db.isNodeId.bind(graphStore.db))
    return codeEditorTooltip(vueHostValue, {
      nodeId,
      syntax: syn.name,
      graphDb: graphStore.db,
      suggestionDbStore,
    })
  })
}
