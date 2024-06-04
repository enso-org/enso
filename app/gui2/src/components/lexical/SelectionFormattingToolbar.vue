<script setup lang="ts">
import ToggleIcon from '@/components/ToggleIcon.vue'
import { type UseFormatting } from '@/components/lexical/formatting'

const props = defineProps<{ formatting: UseFormatting }>()

const { bold, italic, strikethrough } = props.formatting
</script>

<template>
  <div class="SelectionFormattingToolbar">
    <ToggleIcon v-bind="bold.state" icon="bold" title="Bold" @update:modelValue="bold.set" />
    <ToggleIcon
      v-bind="italic.state"
      icon="italic"
      title="Italic"
      @update:modelValue="italic.set"
    />
    <ToggleIcon
      v-bind="strikethrough.state"
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
