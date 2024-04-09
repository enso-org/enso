import { createContextStore } from '@/providers'
import { useGraphStore } from '@/stores/graph'
import { type NodeId } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import type { Icon } from '@/util/iconName'
import { computed, proxyRefs, type Ref } from 'vue'
import type { WidgetEditHandler } from './widgetRegistry/editHandler'

function makeExistenceRegistry(onChange: (anyExist: boolean) => void) {
  const registered = new Set<number>()
  let nextId = 0
  return {
    register: () => {
      const id = nextId++
      if (registered.size === 0) onChange(true)
      registered.add(id)
      return {
        unregister: () => {
          registered.delete(id)
          if (registered.size === 0) onChange(false)
        },
      }
    },
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
    conditionalPorts: Ref<Set<Ast.AstId>>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    currentEdit: Ref<WidgetEditHandler | undefined>,
    emitOpenFullMenu: () => void,
    clippingInhibitorsChanged: (anyExist: boolean) => void,
  ) => {
    const graph = useGraphStore()
    const nodeSpanStart = computed(() => graph.moduleSource.getSpan(astRoot.value.id)![0])
    const { register: inhibitClipping } = makeExistenceRegistry(clippingInhibitorsChanged)
    return proxyRefs({
      astRoot,
      nodeId,
      icon,
      connectedSelfArgumentId,
      potentialSelfArgumentId,
      conditionalPorts,
      extended,
      nodeSpanStart,
      hasActiveAnimations,
      currentEdit,
      emitOpenFullMenu,
      inhibitClipping,
    })
  },
)
