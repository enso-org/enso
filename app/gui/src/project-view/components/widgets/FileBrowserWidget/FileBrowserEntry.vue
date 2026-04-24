<script setup lang="ts">
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { ref, watch } from 'vue'

const props = defineProps<{
  title: string
  icon: Icon
  highlighted?: boolean
  editingState?: 'editing' | 'pending' | 'just created' | undefined
}>()

const emit = defineEmits<{
  nameAccepted: [string]
}>()

const currentTitle = ref(props.title)
const input = ref<HTMLInputElement>()
watch(
  () => props.title,
  (newTitle) => (currentTitle.value = newTitle),
)
watch(input, (newInput) => {
  if (newInput != null) {
    newInput.focus()
    newInput.select()
  }
})
</script>

<template>
  <div :class="{ FileBrowserEntry: true, highlighted, clickable: true }">
    <GrowingSpinner v-if="editingState === 'pending'" :size="16" phase="loading-medium" />
    <SvgIcon v-else :name="icon" />
    <input
      v-if="editingState === 'editing'"
      ref="input"
      v-model="currentTitle"
      @blur="emit('nameAccepted', currentTitle)"
      @keydown.enter.stop="input?.blur()"
      @pointerdown.stop
      @click.stop
      @contextmenu.stop
      @keydown.backspace.stop
      @keydown.delete.stop
      @keydown.arrow-left.stop
      @keydown.arrow-right.stop
    />
    <div v-else>{{ title }}</div>
  </div>
</template>

<style scoped>
.FileBrowserEntry {
  width: 100%;
  justify-content: start;
  display: flex;
  align-items: center;
  padding: 4px;
  border-radius: var(--radius-full);
  border: none;
  transition: background-color 0.3s;
  margin: -4px;
  gap: 4px;
  &:hover,
  &:focus,
  &:active {
    background-color: var(--color-menu-entry-hover-bg);
  }

  &.highlighted {
    background-color: var(--color-menu-entry-selected-bg);
  }

  & .LoadingSpinner {
    border-radius: 100%;
  }
}
</style>
