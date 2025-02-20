<script setup lang="ts">
import { useResizeObserver } from '@/composables/events'
import { useGraphEditorLayers } from '@/providers/graphEditorLayers'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { endOnClickOutside } from '@/util/autoBlur'
import { autoUpdate, flip, shift, useFloating } from '@floating-ui/vue'
import { computed, onMounted, useTemplateRef, watch } from 'vue'

const { point } = defineProps<{
  /** Location to display the menu near, in client coordinates. */
  point: { x: number; y: number }
}>()
const emit = defineEmits<{ close: [] }>()

const interaction = injectInteractionHandler()
const { floating: floatingLayer } = useGraphEditorLayers()

const menu = useTemplateRef<HTMLElement>('menu')

function pointVirtualEl({ x, y }: { x: number; y: number }) {
  return {
    getBoundingClientRect() {
      return {
        width: 0,
        height: 0,
        x,
        y,
        top: y,
        left: x,
        right: x,
        bottom: y,
      }
    },
  }
}

const virtualEl = computed(() => pointVirtualEl(point))
const { floatingStyles, update } = useFloating(virtualEl, menu, {
  placement: 'bottom-start',
  middleware: [flip(), shift({ crossAxis: true })],
  whileElementsMounted: autoUpdate,
})

const menuSize = useResizeObserver(menu)
watch(menuSize, update)

onMounted(() => {
  interaction.setCurrent(
    endOnClickOutside(menu, {
      cancel: () => emit('close'),
      end: () => emit('close'),
    }),
  )
})
</script>

<template>
  <Teleport v-if="floatingLayer" :to="floatingLayer">
    <div ref="menu" class="PointFloatingMenu" :style="floatingStyles">
      <slot />
    </div>
  </Teleport>
</template>

<style>
.PointFloatingMenu {
  position: absolute;
  top: 0;
  left: 0;
  height: fit-content;
  width: fit-content;
}
</style>
