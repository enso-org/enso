<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import CircularMenu from '@/components/CircularMenu.vue'

const isAutoEvaluationDisabled = ref(false)
const isDocsVisible = ref(false)
const isVisualizationVisible = ref(false)
</script>

<template>
  <Story
    title="Circular Menu"
    group="graph"
    :layout="{ type: 'grid', width: 200 }"
    autoPropsDisabled
  >
    <div style="height: 76px">
      <div style="position: absolute; left: 32px">
        <CircularMenu
          v-model:is-auto-evaluation-disabled="isAutoEvaluationDisabled"
          v-model:is-docs-visible="isDocsVisible"
          v-model:is-visualization-visible="isVisualizationVisible"
          @update:isAutoEvaluationDisabled="logEvent('update:isAutoEvaluationDisabled', [$event])"
          @update:isDocsVisible="logEvent('update:isDocsVisible', [$event])"
          @update:isVisualizationVisible="logEvent('update:isVisualizationVisible', [$event])"
        />
      </div>
    </div>

    <template #controls>
      <HstCheckbox v-model="isAutoEvaluationDisabled" title="isAutoEvaluationDisabled" />
      <HstCheckbox v-model="isDocsVisible" title="isDocsVisible" />
      <HstCheckbox v-model="isVisualizationVisible" title="isVisualizationVisible" />
    </template>
  </Story>
</template>
