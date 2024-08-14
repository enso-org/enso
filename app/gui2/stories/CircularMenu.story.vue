<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import CircularMenu from '@/components/CircularMenu.vue'

const isRecordingOverridden = ref(false)
const isVisualizationVisible = ref(false)

const emptySet = new Set<string>()
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
          v-model:isRecordingOverridden="isRecordingOverridden"
          v-model:isVisualizationEnabled="isVisualizationVisible"
          :isRecordingEnabledGlobally="true"
          :isFullMenuVisible="true"
          :isRemovable="true"
          :matchableNodeColors="emptySet"
          :documentationUrl="undefined"
          @update:isRecordingOverridden="logEvent('update:isRecordingOverridden', [$event])"
          @update:isVisualizationVisible="logEvent('update:isVisualizationVisible', [$event])"
        />
      </div>
    </div>

    <template #controls>
      <HstCheckbox v-model="isRecordingOverridden" title="isRecordingOverridden" />
      <HstCheckbox v-model="isVisualizationVisible" title="isVisualizationVisible" />
    </template>
  </Story>
</template>
