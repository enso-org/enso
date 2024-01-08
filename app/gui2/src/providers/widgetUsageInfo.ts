import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import type { WidgetComponent, WidgetInput } from '@/providers/widgetRegistry'
import { identity } from '@vueuse/core'

export { injectFn as injectWidgetUsageInfo, provideFn as provideWidgetUsageInfo }
const { provideFn, injectFn } = createContextStore('Widget usage info', identity<WidgetUsageInfo>)

/**
 * Information about a widget that can be accessed in its child views. Currently this is used during
 * widget selection to prevent the same widget type from being rendered multiple times on the same
 * AST node.
 */
interface WidgetUsageInfo {
  /**
   * An object which is used to distinguish between distinct nodes in a widget tree. When selecting
   * a widget type for an input value with the same `usageKey` as in parent widget, the widget types
   * that were previously used for this input value are not considered for selection. The key is
   * determined by {@link usageKeyForInput} method - currently it's just the widget's port Id.
   */
  usageKey: unknown
  /** All widget types that were rendered so far using the same AST node. */
  previouslyUsed: Set<WidgetComponent<any>>
  updateHandler: (value: unknown, origin: PortId) => void
  nesting: number
}

/**
 * Get usage key for given input. See {@link WidgetUsageInfo} for details.
 */
export function usageKeyForInput(widget: WidgetInput): unknown {
  return widget.portId
}
