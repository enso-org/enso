<script setup lang="ts">
import { ref } from 'vue'
import { Action, Actions } from '../providers/action'
import ContextMenu from './ContextMenu.vue'

const { actions } = defineProps<{
  actions: (Action | keyof Actions)[]
}>()

const point = ref<{ x: number; y: number } | null>(null)
</script>

<template>
  <slot @contextmenu="point = $event" />
  <ContextMenu v-if="point != null" :actions="actions" :point="point" @close="point = null">
    <slot name="menuElements" />
  </ContextMenu>
</template>
