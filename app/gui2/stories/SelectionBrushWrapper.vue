<script setup lang="ts">
import { computed, ref, shallowRef } from 'vue'

import { usePointer } from '@/composables/events'
import { useKeyboard } from '@/composables/keyboard'
import { useNavigator } from '@/composables/navigator'
import type { Vec2 } from '@/util/data/vec2'

const viewportNode = ref<HTMLElement>()

const keyboard = useKeyboard()
const navigator = useNavigator(viewportNode, keyboard)

const selectionAnchor = shallowRef<Vec2>()

const selection = usePointer((_, __, eventType) => {
  if (selection.dragging && selectionAnchor.value == null) {
    selectionAnchor.value = navigator.sceneMousePos?.copy()
  } else if (eventType === 'stop') {
    selectionAnchor.value = undefined
  }
})

const scaledMousePos = computed(() => navigator.sceneMousePos?.scale(navigator.scale))
const scaledSelectionAnchor = computed(() => selectionAnchor.value?.scale(navigator.scale))
</script>

<template>
  <div
    ref="viewportNode"
    style="cursor: none; height: 100%"
    v-on.="navigator.pointerEvents"
    v-on..="selection.events"
  >
    <slot
      :scaledMousePos="scaledMousePos"
      :scaledSelectionAnchor="scaledSelectionAnchor"
      :navigator="navigator"
    ></slot>
  </div>
</template>
