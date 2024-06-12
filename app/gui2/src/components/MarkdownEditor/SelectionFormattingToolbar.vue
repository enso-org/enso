<script setup lang="ts">
import { type UseFormatting } from '@/components/MarkdownEditor/formatting'
import ToggleIcon from '@/components/ToggleIcon.vue'

const props = defineProps<{ formatting: UseFormatting }>()

const { bold, italic, strikethrough } = props.formatting
</script>

<template>
  <div class="SelectionFormattingToolbar">
    <ToggleIcon
      :modelValue="bold.state.value"
      icon="bold"
      title="Bold"
      @update:modelValue="bold.set"
    />
    <ToggleIcon
      :modelValue="italic.state.value"
      icon="italic"
      title="Italic"
      @update:modelValue="italic.set"
    />
    <ToggleIcon
      :modelValue="strikethrough.state.value"
      icon="strike-through"
      title="Strikethrough"
      @update:modelValue="strikethrough.set"
    />
  </div>
</template>

<style scoped>
.SelectionFormattingToolbar {
  display: flex;
  background-color: white;
  border-radius: var(--radius-full);
  padding: 4px;
  gap: 4px;
}
</style>
