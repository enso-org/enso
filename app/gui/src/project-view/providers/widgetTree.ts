import { createContextStore } from '@/providers'
import { type WidgetEditHandlerRoot } from '@/providers/widgetRegistry/editHandler'
import { PrimaryApplication } from '@/stores/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { Opt } from '@/util/data/opt'
import { proxyRefs } from '@/util/reactivity'
import { computed, shallowRef, type Ref, type ShallowUnwrapRef } from 'vue'
import { ExternalId } from 'ydoc-shared/yjsModel'

export const [provideWidgetTree, injectWidgetTree] = createContextStore(
  'Widget tree',
  (
    externalId: Ref<ExternalId>,
    rootElement: Ref<Opt<HTMLElement>>,
    conditionalPorts: Ref<Set<Ast.AstId> | undefined>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    primaryApplication: Ref<PrimaryApplication>,
  ) => {
    const { setCurrentEditRoot, currentEdit } = useCurrentEdit()
    return proxyRefs({
      externalId,
      rootElement,
      conditionalPorts,
      extended,
      hasActiveAnimations,
      primaryApplication,
      setCurrentEditRoot,
      currentEdit,
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
