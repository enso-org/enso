<script lang="ts">
import ResizeHandles from '@/components/ResizeHandles.vue'
import { useResizeObserver } from '@/composables/events'
import { createContextStore } from '@/providers'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { BoundsSet, Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, Ref, toValue, watch, WatchSource } from 'vue'
import { NODE_CONTENT_PADDING } from './GraphNode.vue'

const [provideResizableWidgetRegistry, injectResizableWidgetRegistry] = createContextStore(
  'ResizableWidgets',
  (nodeWidth: Ref<number | null>, widgetTreeWidth: WatchSource<number>) => {
    const registeredResizables = new Map<string, [Ref<Rect>, Ref<Vec2>]>()
    const resizablesCount = ref(0)

    function register(metadataKey: string, rect: Ref<Rect>, domSize: Ref<Vec2>) {
      registeredResizables.set(metadataKey, [rect, domSize])
      resizablesCount.value = registeredResizables.size
    }

    function unregister(metadataKey: string) {
      registeredResizables.delete(metadataKey)
      resizablesCount.value = registeredResizables.size
    }

    function adjustToNodeWidth(nodeWidth: number) {
      if (resizablesCount.value === 1) {
        const change = nodeWidth - NODE_CONTENT_PADDING * 2 - toValue(widgetTreeWidth)
        const widgetBounds = registeredResizables.values().next().value
        if (widgetBounds != null) {
          const [bounds, domSize] = widgetBounds
          bounds.value = new Rect(
            Vec2.Zero,
            new Vec2(domSize.value.x + change, bounds.value.height),
          )
        }
      }
    }

    watch([nodeWidth, resizablesCount, widgetTreeWidth], ([nodeWidth]) => {
      if (nodeWidth) {
        adjustToNodeWidth(nodeWidth)
      }
    })

    let initialNodeWidthOnWidgetDrag: number | null = null

    return {
      register,
      unregister,
      visResizeHandleEvents: {
        'update:modelValue': (newRect: Rect) => {
          adjustToNodeWidth(newRect.width)
        },
      },
      widgetResizeHandleEvents: {
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

export { provideResizableWidgetRegistry, injectResizableWidgetRegistry }
</script>

<script setup lang="ts">
const props = defineProps<{
  input: WidgetInput
  metadataKey: string
  config: { size: { x: number; y: number } }
  onUpdate: (update: WidgetUpdate) => void
}>()

const size = computed(() => Vec2.FromXY(props.config.size))
const graphNav = injectGraphNavigator()
const htmlRoot = ref<HTMLElement>()

const htmlRootSize = useResizeObserver(htmlRoot)

const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, size.value.scale(graphNav.scale))
  },
  set(value) {
    props.onUpdate({
      portUpdate: {
        origin: props.input.portId,
        metadataKey: 'WidgetTableEditor',
        metadata: {
          ...props.config,
          size: {
            x: value.width / graphNav.scale,
            y: value.height / graphNav.scale,
          },
        },
      },
      directInteraction: false,
    })
  },
})

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    height: `${size.value.y}px`,
    minWidth: 0,
  }
})

const registry = injectResizableWidgetRegistry(true)

watch(
  () => props.input.portId,
  (key, _, onCleanup) => {
    registry?.register(key, clientBounds, htmlRootSize)
    onCleanup(() => registry?.unregister(key))
  },
  { immediate: true },
)
</script>

<template>
  <div ref="htmlRoot" :style="widgetStyle">
    <slot />
    <ResizeHandles v-model="clientBounds" bottom right v-on="registry?.widgetResizeHandleEvents" />
  </div>
</template>
