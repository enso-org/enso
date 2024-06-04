<script setup lang="ts">
import ToggleIcon from '@/components/ToggleIcon.vue'
import { type UseFormatting } from '@/components/lexical/formatting'

const props = defineProps<{ formatting: UseFormatting }>()

const { bold, italic, strikethrough } = props.formatting
</script>

<template>
  <div class="SelectionFormattingToolbar">
    <ToggleIcon v-bind="bold.state" @update:modelValue="bold.set" icon="bold" title="Bold" />
    <ToggleIcon
      v-bind="italic.state"
      @update:modelValue="italic.set"
      icon="italic"
      title="Italic"
    />
    <ToggleIcon
      v-bind="strikethrough.state"
      @update:modelValue="strikethrough.set"
      icon="strike-through"
      title="Strikethrough"
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
