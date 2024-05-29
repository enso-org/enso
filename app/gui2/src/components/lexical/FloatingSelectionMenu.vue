<script setup lang="ts">
import { useSelectionBounds } from '@/composables/domSelection'
import { flip, offset, useFloating } from '@floating-ui/vue'
import type { MaybeElement } from '@vueuse/core'
import { computed, shallowRef, toRef } from 'vue'

const props = defineProps<{ selectionElement: MaybeElement }>()

const rootElement = shallowRef<HTMLElement>()

const { bounds: selectionBounds } = useSelectionBounds(toRef(props, 'selectionElement'))
const virtualElement = computed(() => {
  const rect = selectionBounds.value?.toDomRect()
  return rect ? { getBoundingClientRect: () => rect } : undefined
})
const { floatingStyles } = useFloating(virtualElement, rootElement, {
  placement: 'top-start',
  middleware: [
    offset({
      mainAxis: 4,
      alignmentAxis: -2,
    }),
    flip(),
  ],
})
</script>

<template>
  <div
    v-if="selectionBounds"
    ref="rootElement"
    :style="floatingStyles"
    class="FloatingSelectionMenu"
  >
    <slot />
  </div>
</template>
