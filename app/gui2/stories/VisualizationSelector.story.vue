<script setup lang="ts">
import { ref } from 'vue'

import VisualizationSelector from '@/components/VisualizationSelector.vue'
import { logEvent } from 'histoire/client'

const types = ref(['JSON', 'Table', 'Scatterplot'])
</script>

<template>
  <Story
    title="Selector"
    group="visualizations"
    :layout="{ type: 'grid', width: 200 }"
    auto-props-disabled
  >
    <VisualizationSelector
      style="color: var(--color-text); position: relative; left: 12px; width: fit-content"
      :types="types"
      @hide="logEvent('hide', [])"
      @update:type="logEvent('update:type', [$event])"
    />

    <template #controls>
      <HstJson v-model="types" title="types" />
    </template>
  </Story>
</template>
