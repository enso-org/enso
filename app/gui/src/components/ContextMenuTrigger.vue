<script setup lang="ts">
import { ref } from 'vue'
import { Action, Actions } from '../providers/action'
import ContextMenu from './ContextMenu.vue'

const { actions } = defineProps<{
  actions: (Action | keyof Actions)[]
}>()

const point = ref<{ x: number; y: number } | null>(null)
const menuComponent = ref<typeof ContextMenu>()

defineExpose({
  menuComponent,
})
</script>

<template>
  <div style="display: contents" @contextmenu="point = $event"><slot /></div>
  <ContextMenu
    v-if="point != null"
    ref="menuComponent"
    :actions="actions"
    :point="point"
    @close="point = null"
  >
    <slot name="menuElements" />
  </ContextMenu>
</template>
