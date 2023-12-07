import { identity } from '@vueuse/core'
import { createContextStore } from '.'
import type { WidgetComponent, WidgetInput } from './widgetRegistry'

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
   * determined by the widget input's method defined on {@link GetUsageKey} symbol key. When no such
   * method is defined, the input value itself is used as the key.
   */
  usageKey: unknown
  /** All widget types that were rendered so far using the same AST node. */
  previouslyUsed: Set<WidgetComponent<any>>
  nesting: number
}

/**
 * A symbol key used for defining a widget input method's usage key. A method with this key can be
 * declared for widget input types that are not unique by themselves, but are just a thin wrapper
 * around another input value, and don't want to be considered as a completely separate entity for
 * the purposes of widget type selection.
 */
export const GetUsageKey = Symbol('GetUsageKey')

export function usageKeyForInput(widget: WidgetInput): unknown {
  if (GetUsageKey in widget && typeof widget[GetUsageKey] === 'function') {
    return widget[GetUsageKey]()
  } else {
    return widget
  }
}
