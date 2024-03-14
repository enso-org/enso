import { createContextStore } from '@/providers'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, proxyRefs, type Ref } from 'vue'

function makeExistenceRegistry(onChange: (anyExist: boolean) => void) {
  const registered = new Set<number>()
  let nextId = 0
  return () => {
    const id = nextId++
    if (registered.size === 0) onChange(true)
    registered.add(id)
    return () => {
      registered.delete(id)
      if (registered.size === 0) onChange(false)
    }
  }
}

export { injectFn as injectWidgetTree, provideFn as provideWidgetTree }
const { provideFn, injectFn } = createContextStore(
  'Widget tree',
  (
    astRoot: Ref<Ast.Ast>,
    nodeId: Ref<NodeId>,
    icon: Ref<Icon>,
    connectedSelfArgumentId: Ref<Ast.AstId | undefined>,
    potentialSelfArgumentId: Ref<Ast.AstId | undefined>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    emitOpenFullMenu: () => void,
    clippingInhibitorsChanged: (anyExist: boolean) => void,
  ) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    const inhibitClipping = makeExistenceRegistry(clippingInhibitorsChanged)
    return proxyRefs({
      astRoot,
      nodeId,
      icon,
      connectedSelfArgumentId,
      potentialSelfArgumentId,
      extended,
      nodeSpanStart,
      hasActiveAnimations,
      emitOpenFullMenu,
      inhibitClipping,
    })
  },
)
