import type { ToValue } from '$/utils/reactivity'
import type { ResizeHandlesEventRegistry } from '@/components/resizeHandles'
import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import type { BoundsSet } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { ref, toValue, watch, type Ref } from 'vue'

/**
 * Registry of resizable widgets.
 *
 * It is provided to context store by every node, and widgets with `ResizableWidget` register here.
 * If a node contains only one resizable widget, it's size is synchronized with visualization's
 * size, so the widget tree covers entire's node width.
 *
 * All sizes should be in "client" space (after zoom applied).
 * @param nodeWidth read-write ref to target node width - may be updated when single
 * visualization is resizing.
 * @param widgetTreeDomWidth the actual current size of wiget tree in DOM (obtained from
 * {@link useResizeObserver})
 */
export function useResizableWidgetRegistry(
  nodeWidth: Ref<number | null>,
  nodePadding: ToValue<number>,
  widgetTreeDomWidth: ToValue<number>,
) {
  const registeredResizables = new Map<PortId, { bounds: Ref<Vec2>; domSize: Ref<Vec2> }>()
  const resizablesCount = ref(0)

  const preferredHeight = ref()

  function computePreferredHeight() {
    let max = 0
    for (const { bounds } of registeredResizables.values()) max = Math.max(max, bounds.value.y)
    return max
  }

  /**
   * Register new resizable component.
   * @param bounds The "target" widget bounds, to which DOM is updated.
   * @param domSize The actual current DOM size, obtained from {@link useResizeObserver}
   */
  function register(portId: PortId, bounds: Ref<Vec2>, domSize: Ref<Vec2>) {
    registeredResizables.set(portId, { bounds, domSize })
    resizablesCount.value = registeredResizables.size
    preferredHeight.value = computePreferredHeight()
  }

  function unregister(portId: PortId) {
    registeredResizables.delete(portId)
    resizablesCount.value = registeredResizables.size
    preferredHeight.value = computePreferredHeight()
  }

  /**
   * If there is only one widget, adjust its width, so the widgetTree will fill the node.
   * @param nodeWidth describes the target nodeWidth (may differ from the actual in case
   * like we want to shorten the node).
   */
  function adjustToNodeWidth(nodeWidth: number) {
    if (resizablesCount.value === 1) {
      const targetWidgetTreeDomWidth = nodeWidth - 2 * toValue(nodePadding)
      const change = targetWidgetTreeDomWidth - toValue(widgetTreeDomWidth)
      const widgetBounds = registeredResizables.values().next().value
      if (widgetBounds != null) {
        const { bounds, domSize } = widgetBounds
        bounds.value = new Vec2(domSize.value.x + change, bounds.value.y)
      }
    }
  }

  watch(
    [nodeWidth, () => toValue(nodePadding), resizablesCount, () => toValue(widgetTreeDomWidth)],
    ([nodeWidth]) => {
      if (nodeWidth) {
        adjustToNodeWidth(nodeWidth)
      }
    },
    { flush: 'post' },
  )

  let initialNodeWidthOnWidgetDrag: number | null = null

  return {
    register,
    unregister,
    connectVisResizeHandleEventHandlers: (callbacks: ResizeHandlesEventRegistry) =>
      callbacks.onResizeWidth(adjustToNodeWidth),
    connectWidgetResizeHandleEventHandlers: (callbacks: ResizeHandlesEventRegistry) => {
      callbacks.onResizingChange((bounds: BoundsSet) => {
        if (bounds.left || bounds.right) {
          initialNodeWidthOnWidgetDrag = nodeWidth.value
        }
      })
      callbacks.onResizeWidth((_width, delta) => {
        if (resizablesCount.value === 1 && initialNodeWidthOnWidgetDrag != null) {
          nodeWidth.value = initialNodeWidthOnWidgetDrag + delta
        }
      })
    },
    preferredHeight,
  }
}

const [provideResizableWidgetRegistry, injectResizableWidgetRegistry] = createContextStore(
  'ResizableWidgets',
  useResizableWidgetRegistry,
)

export { injectResizableWidgetRegistry, provideResizableWidgetRegistry }
