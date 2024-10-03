<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SvgButton from '@/components/SvgButton.vue'

const emit = defineEmits<{
  createNodes: [options: NodeCreationOptions[]]
}>()

function addNode() {
  emit('createNodes', [{ commit: false, content: undefined }])
}
</script>

<template>
  <SvgButton name="add" class="SmallPlusButton" title="Add Component" @click.stop="addNode" />
</template>

<style scoped>
.SmallPlusButton {
  width: var(--node-base-height);
  height: var(--node-base-height);
  margin: 0;

  backdrop-filter: var(--blur-app-bg);
  background: var(--color-app-bg);
  border-radius: 16px;

  &:hover {
    background: rgb(230, 230, 255);
  }
  &:active {
    background: rgb(158, 158, 255);
  }
}
</style>
