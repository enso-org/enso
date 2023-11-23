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
  input: WidgetInput
  /** All widget types that were rendered so far using the same AST node. */
  previouslyUsed: Set<WidgetComponent<any>>
  nesting: number
}
