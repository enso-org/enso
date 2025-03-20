<script setup lang="ts">
import ResizeHandles from '@/components/ResizeHandles.vue'
import { createContextStore } from '@/providers'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { PortId } from '@/providers/portInfo'
import { WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed } from 'vue'

const props = defineProps<{
  input: WidgetInput
  metadataKey: string
  config: { size: { x: number; y: number } }
  onUpdate: (update: WidgetUpdate) => void
}>()

const size = computed(() => Vec2.FromXY(props.config.size))
const graphNav = injectGraphNavigator()

const [provideResizableWidgetRegistry, injectResizableWidgetRegistry] = createContextStore(
  'ResizableWidgets',
  () => {
    const registeredResizables = new Map<string, PortId>()

    function register(metadataKey: string, portId: PortId) {
      registeredResizables.set(metadataKey, portId)
    }

    function unregister(metadataKey: string) {
      registeredResizables.delete(metadataKey)
    }

    return {
      register,
      unregister,
    }
  },
)

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
  }
})
</script>

<template>
  <div :style="widgetStyle">
    <slot />
    <ResizeHandles v-model="clientBounds" bottom right />
  </div>
</template>
