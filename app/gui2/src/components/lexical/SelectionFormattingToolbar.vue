<script setup lang="ts">
import ToggleIcon from '@/components/ToggleIcon.vue'
import FloatingSelectionMenu from '@/components/lexical/FloatingSelectionMenu.vue'
import { useFormatting } from '@/components/lexical/formatting'
import type { LexicalEditor } from 'lexical'
import { onMounted, ref } from 'vue'

const props = defineProps<{ editor: LexicalEditor }>()

const { bold, italic, strikethrough } = useFormatting(props.editor)

const editorRoot = ref<HTMLElement>()

onMounted(() => {
  editorRoot.value = props.editor.getRootElement() ?? undefined
})
</script>

<template>
  <FloatingSelectionMenu
    v-if="editorRoot"
    :selectionElement="editorRoot"
    :verticalPaddingPx="4"
    :horizontalOffsetPx="-2"
  >
    <div class="SelectionFormattingToolbar">
      <div class="button">
        <ToggleIcon v-model="bold" icon="random" />
      </div>
      <div class="button">
        <ToggleIcon v-model="italic" icon="heart" />
      </div>
      <div class="button">
        <ToggleIcon v-model="strikethrough" icon="unstable2" />
      </div>
    </div>
  </FloatingSelectionMenu>
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
