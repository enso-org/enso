<script setup lang="ts">
import MenuPanel from '@/components/MenuPanel.vue'
import { Action, Actions } from '../providers/action'
import MenuEntry from './MenuEntry.vue'

const { actions } = defineProps<{
  actions: (Action | keyof Actions)[]
}>()
const emit = defineEmits<{ close: [] }>()
</script>

<template>
  <MenuPanel ref="menu" class="ComponentContextMenu">
    <MenuEntry
      v-for="(action, index) of actions"
      :key="index"
      :action="action"
      @click.stop="emit('close')"
    />
    <slot />
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
