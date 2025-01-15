<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useNodeExecution } from '@/stores/project/nodeExecution'
import ControlButtons from './ControlButtons.vue'

const nodeExecution = useNodeExecution()
</script>

<template>
  <ControlButtons class="RecordControl">
    <template #left>
      <SvgButton
        title="Refresh"
        class="iconButton"
        name="refresh"
        draggable="false"
        @click.stop="nodeExecution.recomputeAll()"
      />
    </template>
    <template #right>
      <SvgButton
        title="Write All"
        class="iconButton"
        name="workflow_play"
        draggable="false"
        @click.stop="nodeExecution.recomputeAll('Live')"
      />
    </template>
  </ControlButtons>
</template>

<style scoped>
.iconButton:active {
  color: #ba4c40;
}
</style>
