<script setup lang="ts">
import MenuEntry from '@/components/MenuEntry.vue'
import MenuPanel from '@/components/MenuPanel.vue'
import type { DisplayableActionName } from '@/providers/action'

const { actions } = defineProps<{
  actions: DisplayableActionName[]
}>()
const emit = defineEmits<{ close: [] }>()
</script>

<template>
  <MenuPanel class="ActionMenu">
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
.ActionMenu {
  margin-top: 2px;
  padding: 4px;
  background: var(--dropdown-opened-background, var(--color-app-bg));
  backdrop-filter: var(--dropdown-opened-backdrop-filter, var(--blur-app-bg));
}
</style>
