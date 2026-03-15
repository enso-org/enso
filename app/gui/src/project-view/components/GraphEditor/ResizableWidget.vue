<script setup lang="ts">
import { type UpdateHandler, WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import ResizeHandles from '@/components/ResizeHandles.vue'
import { useResizeHandles } from '@/components/resizeHandles'
import { injectGraphNavigator } from '@/providers/graphNavigator'
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

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    ['--preferred-height']: `${size.value.y}px`,
  }
})

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
</script>

<template>
  <div class="ResizableWidget" :style="widgetStyle">
    <slot />
    <ResizeHandles right v-on="resizeHandles.events" />
  </div>
</template>

<style>
.ResizableWidget {
  min-width: 32px;
  /* Resizable widgets take the full height of the node. */
  height: 100%;
  /* Non-static so that the absolutely-positioned resize handles will be at the edge of the widget. */
  position: relative;

  min-height: var(--preferred-height);
  .nodeHeightOverridden & {
    min-height: unset;
  }
}
</style>
