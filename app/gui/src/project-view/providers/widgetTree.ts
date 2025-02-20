import { createContextStore } from '@/providers'
import { type WidgetEditHandlerRoot } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { computed, proxyRefs, shallowRef, type Ref, type ShallowUnwrapRef } from 'vue'
import { AstId } from 'ydoc-shared/ast'
import { ExternalId } from 'ydoc-shared/yjsModel'

export const [provideWidgetTree, injectWidgetTree] = createContextStore(
  'Widget tree',
  (
    externalId: Ref<ExternalId>,
    rootElement: Ref<HTMLElement | undefined>,
    conditionalPorts: Ref<Set<Ast.AstId> | undefined>,
    extended: Ref<boolean>,
    hasActiveAnimations: Ref<boolean>,
    potentialSelfArgumentId: Ref<AstId | undefined>,
  ) => {
    const { setCurrentEditRoot, currentEdit } = useCurrentEdit()
    return proxyRefs({
      externalId,
      rootElement,
      conditionalPorts,
      extended,
      hasActiveAnimations,
      potentialSelfArgumentId,
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
