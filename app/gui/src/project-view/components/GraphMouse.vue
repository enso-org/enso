<script setup lang="ts">
import SelectionBrush from '@/components/SelectionBrush.vue'
import { useEvent } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { computed, ref } from 'vue'

const navigator = injectGraphNavigator(true)
const nodeSelection = injectGraphSelection(true)
const scaledSelectionAnchor = computed(() => nodeSelection?.anchor?.scale(navigator?.scale ?? 1))
const scaledSelectionFocus = computed(() => nodeSelection?.focus?.scale(navigator?.scale ?? 1))
const isNativeDragging = ref(0)

useEvent(
  window,
  'dragenter',
  () => {
    isNativeDragging.value += 1
  },
  { capture: true },
)
useEvent(
  window,
  'dragleave',
  () => {
    isNativeDragging.value -= 1
  },
  { capture: true },
)
useEvent(
  window,
  'drop',
  () => {
    isNativeDragging.value -= 1
  },
  { capture: true },
)
</script>

<template>
  <SelectionBrush
    v-if="!isNativeDragging"
    :transform="navigator?.prescaledTransform"
    :position="scaledSelectionFocus"
    :anchor="scaledSelectionAnchor"
  />
</template>
