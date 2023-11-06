import type { AstExtended } from '@/util/ast'
import { computed, proxyRefs, type Ref } from 'vue'
import { createContextStore } from '.'
import type { WidgetComponent } from './widgetRegistry'

export { injectFn as injectWidgetUsageInfo, provideFn as provideWidgetUsageInfo }
const { provideFn, injectFn } = createContextStore(
  'Widget usage info',
  (
    parentInfo: WidgetUsageInfo | undefined,
    ast: Ref<AstExtended>,
    selectedComponent: Ref<WidgetComponent | undefined>,
  ): WidgetUsageInfo => {
    return proxyRefs({
      ast,
      set: computed(() => {
        const set = new Set(parentInfo && parentInfo.ast === ast.value ? parentInfo.set : undefined)
        if (selectedComponent.value != null) set.add(selectedComponent.value)
        return set
      }),
    })
  },
)

interface WidgetUsageInfo {
  ast: AstExtended
  set: Set<WidgetComponent>
}
