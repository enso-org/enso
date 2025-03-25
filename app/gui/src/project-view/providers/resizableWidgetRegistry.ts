import { createContextStore } from '@/providers'
import { BoundsSet, Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { ref, Ref, toValue, watch, WatchSource } from 'vue'

const NODE_CONTENT_PADDING = 4

/**
 * Context Store with registry of resizable widgets.
 *
 * It is provided by every node, and widgets with `ResizableWidget` register here. If a node
 * contains only one resizable widget, it's size is synchronized with visualization's size, so
 * the widget tree covers entire's node width.
 */
const [provideResizableWidgetRegistry, injectResizableWidgetRegistry] = createContextStore(
  'ResizableWidgets',
  (nodeWidth: Ref<number | null>, widgetTreeDomWidth: WatchSource<number>) => {
    const registeredResizables = new Map<string, { bounds: Ref<Rect>; domSize: Ref<Vec2> }>()
    const resizablesCount = ref(0)

    function register(metadataKey: string, bounds: Ref<Rect>, domSize: Ref<Vec2>) {
      registeredResizables.set(metadataKey, { bounds, domSize })
      resizablesCount.value = registeredResizables.size
    }

    function unregister(metadataKey: string) {
      registeredResizables.delete(metadataKey)
      resizablesCount.value = registeredResizables.size
    }

    /**
     * If there is only one widget, adjust its width, so the widgetTree will fill the node.
     * @param nodeWidth describes the target nodeWidth (may differ from the actual in case
     * like we want to shorten the node).
     */
    function adjustToNodeWidth(nodeWidth: number) {
      if (resizablesCount.value === 1) {
        const change = nodeWidth - NODE_CONTENT_PADDING * 2 - toValue(widgetTreeDomWidth)
        console.log('CHANGE', change)
        const widgetBounds = registeredResizables.values().next().value
        if (widgetBounds != null) {
          const { bounds, domSize } = widgetBounds
          bounds.value = new Rect(
            Vec2.Zero,
            new Vec2(domSize.value.x + change, bounds.value.height),
          )
        }
      }
    }

    watch(
      [nodeWidth, resizablesCount, widgetTreeDomWidth],
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
      visResizeHandleEventHandlers: {
        'update:modelValue': (newRect: Rect) => {
          adjustToNodeWidth(newRect.width)
        },
      },
      widgetResizeHandleEventHandlers: {
        'update:resizing': (bounds: BoundsSet) => {
          if (bounds.left || bounds.right) {
            initialNodeWidthOnWidgetDrag = nodeWidth.value
          }
        },
        'update:modelValue': (_: Rect, delta: Vec2) => {
          if (resizablesCount.value === 1 && initialNodeWidthOnWidgetDrag != null) {
            nodeWidth.value = initialNodeWidthOnWidgetDrag + delta.x
          }
        },
      },
    }
  },
)

export { injectResizableWidgetRegistry, provideResizableWidgetRegistry }
