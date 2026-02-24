<script setup lang="ts">
import { type UpdateHandler, WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import ResizeHandles from '@/components/ResizeHandles.vue'
import { useResizeHandles } from '@/components/resizeHandles'
import { useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectResizableWidgetRegistry } from '@/providers/resizableWidgetRegistry'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, toRef, watch } from 'vue'

const props = defineProps<{
  input: WidgetInput
  metadataKey: string
  config: { size: { x: number; y: number } }
  updateCallback: UpdateHandler
}>()

const size = ref(Vec2.Zero)
watch(
  () => props.config.size,
  (value) => (size.value = Vec2.FromXY(value)),
  { immediate: true },
)
const graphNav = injectGraphNavigator()
const htmlRoot = ref<HTMLElement>()

const htmlRootSize = useResizeObserver(htmlRoot)

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    height: '100%',
    minWidth: '32px',
  }
})

const registry = injectResizableWidgetRegistry(true)

watch(
  () => props.input.portId,
  (key, _, onCleanup) => {
    registry?.register(key, size, htmlRootSize)
    onCleanup(() => registry?.unregister(key))
  },
  { immediate: true },
)

const resizeHandles = useResizeHandles({
  size,
  scale: toRef(graphNav, 'scale'),
})
resizeHandles.onResize((value) => {
  if (value.equalsApproximately(size.value, 0.01)) return
  props.updateCallback({
    portUpdate: {
      origin: props.input.portId,
      metadataKey: 'WidgetTableEditor',
      metadata: {
        ...props.config,
        size: value.xy(),
      },
    },
    directInteraction: false,
  })
})
registry?.connectWidgetResizeHandleEventHandlers(resizeHandles)
</script>

<template>
  <div ref="htmlRoot" :style="widgetStyle">
    <slot />
    <ResizeHandles bottom right v-on="resizeHandles.events" />
  </div>
</template>
