<script setup lang="ts">
import BlockTypeMenu from '@/components/MarkdownEditor/BlockTypeMenu.vue'
import FormatPropertiesBar from '@/components/MarkdownEditor/FormatPropertiesBar.vue'
import type { UseFormatting } from '@/components/MarkdownEditor/formatting'

const props = defineProps<{ formatting: UseFormatting }>()

const { blockType } = props.formatting
</script>

<template>
  <div class="FormattingToolbar">
    <div class="section">
      <BlockTypeMenu :modelValue="blockType.state.value" @update:modelValue="blockType.set" />
    </div>
    <div class="section">
      <FormatPropertiesBar :formatting="formatting" />
    </div>
  </div>
</template>

<style scoped>
.FormattingToolbar {
  --color-frame-bg: white;
  display: flex;
  height: 100%;
  flex-direction: row;
  align-items: center;
  gap: 6px;
  line-height: 16px;
}

.section {
  display: flex;
  background-color: white;
  border-radius: var(--radius-full);
  gap: 4px;
  padding: 4px;
}

.FormattingToolbar :deep(.iconLabel) {
  margin-left: 4px;
  padding-right: 4px;
}
</style>
