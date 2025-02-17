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
