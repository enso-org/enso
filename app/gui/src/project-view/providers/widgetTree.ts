import { createContextStore } from '@/providers'
import { type WidgetEditHandlerRoot } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import type { Vec2 } from '@/util/data/vec2'
import { computed, proxyRefs, shallowRef, type Ref, type ShallowUnwrapRef } from 'vue'

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (
    astRoot: Ref<Ast.Ast>,
    nodeId: Ref<NodeId>,
    nodeElement: Ref<HTMLElement | undefined>,
    nodeSize: Ref<Vec2>,
    potentialSelfArgumentId: Ref<Ast.AstId | undefined>,
    conditionalPorts: Ref<Set<Ast.AstId>>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    emitOpenFullMenu: () => void,
  ) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    const { setCurrentEditRoot, currentEdit } = useCurrentEdit()
    return proxyRefs({
      astRoot,
      nodeId,
      nodeElement,
      nodeSize,
      potentialSelfArgumentId,
      conditionalPorts,
      extended,
      nodeSpanStart,
      hasActiveAnimations,
      setCurrentEditRoot,
      currentEdit,
      emitOpenFullMenu,
    })
  },
)

/** TODO: Add docs */
export function useCurrentEdit() {
  const currentEditRoot = shallowRef<WidgetEditHandlerRoot>()
  return {
    currentEdit: computed(() => currentEditRoot.value?.currentEdit()),
    setCurrentEditRoot: (root: WidgetEditHandlerRoot) => {
      currentEditRoot.value = root
    },
  }
}
export type CurrentEdit = ShallowUnwrapRef<ReturnType<typeof useCurrentEdit>>
