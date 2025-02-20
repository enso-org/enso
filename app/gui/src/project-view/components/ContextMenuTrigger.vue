<script setup lang="ts">
import { ref } from 'vue'
import { Action, ActionName } from '../providers/action'
import ContextMenu from './ContextMenu.vue'

const { actions } = defineProps<{
  actions: (Action | ActionName)[]
}>()
const emit = defineEmits<{
  shown: []
  hidden: []
}>()

const point = ref<{ x: number; y: number } | null>(null)
const menuComponent = ref<typeof ContextMenu>()

function show(at: typeof point.value) {
  point.value = at
  emit('shown')
}

function hide() {
  point.value = null
  emit('hidden')
}
</script>

<template>
  <div style="display: contents" @contextmenu.stop.prevent="show">
    <slot />
    <ContextMenu
      v-if="point != null"
      ref="menuComponent"
      :actions="actions"
      :point="point"
      @close="hide"
    >
      <slot name="menuElements" />
    </ContextMenu>
  </div>
</template>
