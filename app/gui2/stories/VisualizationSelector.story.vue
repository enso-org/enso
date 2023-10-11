<script setup lang="ts">
import { ref } from 'vue'

import VisualizationSelector from '@/components/VisualizationSelector.vue'
import { logEvent } from 'histoire/client'
import type { VisualizationIdentifier } from 'shared/yjsModel'

const type = ref<VisualizationIdentifier>({
  module: { kind: 'Builtin' },
  name: 'Example',
})

const types = ref<VisualizationIdentifier[]>([
  {
    module: { kind: 'Builtin' },
    name: 'Example',
  },
  {
    module: { kind: 'Builtin' },
    name: 'Types',
  },
  {
    module: { kind: 'Builtin' },
    name: 'Here',
  },
])
</script>

<template>
  <Story
    title="Selector"
    group="visualizations"
    :layout="{ type: 'grid', width: 200 }"
    autoPropsDisabled
  >
    <VisualizationSelector
      v-model="type"
      style="color: var(--color-text); position: relative; left: 12px; width: fit-content"
      :types="types"
      @hide="logEvent('hide', [])"
      @update:modelValue="logEvent('update:modelValue', [JSON.stringify($event)])"
    />

    <template #controls>
      <HstJson v-model="types" title="types" />
    </template>
  </Story>
</template>
