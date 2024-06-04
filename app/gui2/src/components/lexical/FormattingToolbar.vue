<script setup lang="ts">
import BlockTypeMenu from '@/components/lexical/BlockTypeMenu.vue'
import FormatPropertiesBar from '@/components/lexical/FormatPropertiesBar.vue'
import type { UseFormatting } from '@/components/lexical/formatting'

const props = defineProps<{ formatting: UseFormatting }>()

const { blockType } = props.formatting
</script>

<template>
  <div class="FormattingToolbar">
    <div class="section">
      <BlockTypeMenu v-bind:modelValue="blockType.state.value" @update:modelValue="blockType.set" />
    </div>
    <div class="section">
      <FormatPropertiesBar :formatting="formatting" />
    </div>
  </div>
</template>

<style scoped>
.FormattingToolbar {
  --color-frame-bg: white;
  position: sticky;
  top: 0;
  display: flex;
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
