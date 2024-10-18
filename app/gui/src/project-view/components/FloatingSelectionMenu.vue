<script setup lang="ts">
import { useSelectionBounds } from '@/composables/domSelection'
import { useResizeObserver } from '@/composables/events'
import { flip, offset, useFloating } from '@floating-ui/vue'
import type { MaybeElement } from '@vueuse/core'
import { computed, shallowRef, toRef, watch } from 'vue'

const props = defineProps<{ selectionElement: MaybeElement }>()

const rootElement = shallowRef<HTMLElement>()
const rootSize = useResizeObserver(rootElement)

const { bounds: selectionBounds, collapsed: selectionCollapsed } = useSelectionBounds(
  toRef(props, 'selectionElement'),
  true,
)
const virtualElement = computed(() => {
  const rect = selectionBounds.value?.toDomRect()
  return rect ? { getBoundingClientRect: () => rect } : undefined
})
const { floatingStyles, update } = useFloating(virtualElement, rootElement, {
  placement: 'top-start',
  middleware: [
    offset({
      mainAxis: 4,
      alignmentAxis: -2,
    }),
    flip(),
  ],
})

watch(rootSize, update)
</script>

<template>
  <div ref="rootElement" :style="floatingStyles" class="FloatingSelectionMenu">
    <slot v-if="selectionBounds" :collapsed="selectionCollapsed" />
  </div>
</template>
