<script setup lang="ts">
import { ref } from 'vue'
import { Action, ActionName } from '../providers/action'
import ContextMenu from './ContextMenu.vue'

const { actions } = defineProps<{
  actions: (Action | ActionName)[]
}>()

const point = ref<{ x: number; y: number } | null>(null)
const menuComponent = ref<typeof ContextMenu>()

defineExpose({
  menuComponent,
})
</script>

<template>
  <div style="display: contents" @contextmenu.stop.prevent="point = $event">
    <slot />
    <ContextMenu
      v-if="point != null"
      ref="menuComponent"
      :actions="actions"
      :point="point"
      @close="point = null"
    >
      <slot name="menuElements" />
    </ContextMenu>
  </div>
</template>
