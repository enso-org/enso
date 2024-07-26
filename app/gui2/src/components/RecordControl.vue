<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useProjectStore } from '@/stores/project'

const project = useProjectStore()
</script>

<template>
  <div class="RecordControl">
    <div class="control left-end">
      <SvgButton
        title="Refresh"
        class="iconButton"
        name="refresh"
        draggable="false"
        @click.stop="project.executionContext.recompute()"
      />
    </div>
    <div class="control right-end">
      <SvgButton
        title="Write All"
        class="iconButton"
        name="workflow_play"
        draggable="false"
        @click.stop="project.executionContext.recompute('all', 'Live')"
      />
    </div>
  </div>
</template>

<style scoped>
.RecordControl {
  user-select: none;
  display: flex;
  place-items: center;
  gap: 1px;
}

.control {
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  padding: 4px 4px;
  width: 42px;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);

  .iconButton {
    margin: 0 0 0 auto;
  }
}

.right-end {
  border-radius: 0 var(--radius-full) var(--radius-full) 0;

  .iconButton {
    margin: 0 auto 0 0;
  }
}

.iconButton:active {
  color: #ba4c40;
}
</style>
