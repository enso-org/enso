<script setup lang="ts">
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useFormatting } from '@/components/lexical/formatting'
import { useSelectionBounds } from '@/composables/domSelection'
import { flip, offset, useFloating } from '@floating-ui/vue'
import type { MaybeElement } from '@vueuse/core'
import type { LexicalEditor } from 'lexical'
import { computed, shallowRef, toRef } from 'vue'

const props = defineProps<{ editor: LexicalEditor; editorRoot: MaybeElement }>()

const rootElement = shallowRef<HTMLElement>()

const { bounds: selectionBounds } = useSelectionBounds(toRef(props, 'editorRoot'))
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

const { bold, italic, strikethrough } = useFormatting(props.editor)
</script>

<template>
  <div
    v-if="selectionBounds"
    ref="rootElement"
    :style="floatingStyles"
    class="SelectionFormattingToolbar"
  >
    <div class="button">
      <ToggleIcon v-model="bold" icon="bold" />
    </div>
    <div class="button">
      <ToggleIcon v-model="italic" icon="italic" />
    </div>
    <div class="button">
      <ToggleIcon v-model="strikethrough" icon="strike-through" />
    </div>
  </div>
</template>

<style scoped>
.SelectionFormattingToolbar {
  display: flex;
  background-color: white;
  border-radius: var(--radius-full);
}

.button {
  border-radius: var(--radius-full);
  padding: 4px;
  color: black;
}
</style>
