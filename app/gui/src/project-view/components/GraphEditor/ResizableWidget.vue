<script setup lang="ts">
import { type UpdateHandler, WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import ResizeHandles from '@/components/ResizeHandles.vue'
import { useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectResizableWidgetRegistry } from '@/providers/resizableWidgetRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, watch } from 'vue'

const props = defineProps<{
  input: WidgetInput
  metadataKey: string
  config: { size: { x: number; y: number } }
  updateCallback: UpdateHandler
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
    const sizeToStore = value.size.scale(1 / graphNav.scale)
    if (sizeToStore.equalsApproximately(size.value, 0.01)) return
    props.updateCallback({
      portUpdate: {
        origin: props.input.portId,
        metadataKey: 'WidgetTableEditor',
        metadata: {
          ...props.config,
          size: sizeToStore.xy(),
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
    <ResizeHandles
      v-model="clientBounds"
      bottom
      right
      v-on="registry?.widgetResizeHandleEventHandlers"
    />
  </div>
</template>
