<script setup lang="ts">
import ComponentButton from '@/components/ComponentButton.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import { type ComponentButtons } from '@/providers/componentButtons'
import { type SelectionButtons } from '@/providers/selectionButtons'

const emit = defineEmits<{ close: [] }>()

const componentButtons: (keyof ComponentButtons)[] = [
  'toggleDocPanel',
  'toggleVisualization',
  'createNewNode',
  'editingComment',
  'recompute',
  'pickColor',
  'enterNode',
  'startEditing',
]
const selectionButtons: (keyof SelectionButtons)[] = ['copy', 'deleteSelected']
const buttons = [...componentButtons, ...selectionButtons]
</script>

<template>
  <MenuPanel class="ComponentContextMenu" @contextmenu.stop.prevent="emit('close')">
    <ComponentButton
      v-for="button in buttons"
      :key="button"
      :button="button"
      @click.stop="emit('close')"
    />
  </MenuPanel>
</template>

<style scoped>
.MenuPanel {
  margin-top: 2px;
  padding: 4px;
  background: var(--dropdown-opened-background, var(--color-app-bg));
  backdrop-filter: var(--dropdown-opened-backdrop-filter, var(--blur-app-bg));
}
</style>
