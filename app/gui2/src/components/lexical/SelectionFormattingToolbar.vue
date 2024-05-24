<script setup lang="ts">
import ToggleIcon from '@/components/ToggleIcon.vue'
import { type UseFormatting } from '@/components/lexical/formatting'

const props = defineProps<{ formatting: UseFormatting }>()

const { bold, italic, strikethrough } = props.formatting
</script>

<template>
  <div class="SelectionFormattingToolbar">
    <ToggleIcon v-model="bold" icon="bold" title="Bold" />
    <ToggleIcon v-model="italic" icon="italic" title="Italic" />
    <ToggleIcon v-model="strikethrough" icon="strike-through" title="Strikethrough" />
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
