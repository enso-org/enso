<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useGraphStore } from '@/stores/graph'
import ControlButtons from './ControlButtons.vue'

const graphStore = useGraphStore()
</script>

<template>
  <ControlButtons class="UndoRedoButtons">
    <template #left>
      <SvgButton
        title="Undo"
        class="iconButton"
        name="undo"
        draggable="false"
        :disabled="!graphStore.undoManager.canUndo.value"
        @click.stop="graphStore.undoManager.undo"
      />
    </template>
    <template #right>
      <SvgButton
        title="Redo"
        class="iconButton"
        name="redo"
        draggable="false"
        :disabled="!graphStore.undoManager.canRedo.value"
        @click.stop="graphStore.undoManager.redo"
      />
    </template>
  </ControlButtons>
</template>
