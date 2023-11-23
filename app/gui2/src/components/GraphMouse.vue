<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { computed } from 'vue'
import SelectionBrush from './SelectionBrush.vue'

const navigator = injectGraphNavigator(true)
const nodeSelection = injectGraphSelection(true)
const scaledMousePos = computed(() => navigator?.sceneMousePos?.scale(navigator?.scale ?? 1))
const scaledSelectionAnchor = computed(() => nodeSelection?.anchor?.scale(navigator?.scale ?? 1))
</script>

<template>
  <SelectionBrush
    v-if="scaledMousePos"
    :position="scaledMousePos"
    :anchor="scaledSelectionAnchor"
    :style="{ transform: navigator?.prescaledTransform }"
  />
</template>
