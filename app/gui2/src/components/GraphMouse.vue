<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useEvent } from '@/util/events'
import { computed, ref } from 'vue'
import SelectionBrush from './SelectionBrush.vue'

const navigator = injectGraphNavigator(true)
const nodeSelection = injectGraphSelection(true)
const scaledMousePos = computed(() => navigator?.sceneMousePos?.scale(navigator?.scale ?? 1))
const scaledSelectionAnchor = computed(() => nodeSelection?.anchor?.scale(navigator?.scale ?? 1))
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
    v-if="scaledMousePos && !isNativeDragging"
    :position="scaledMousePos"
    :anchor="scaledSelectionAnchor"
    :style="{ transform: navigator?.prescaledTransform }"
  />
</template>
