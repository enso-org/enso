<script setup lang="ts">
import { useConainerData } from '$/providers/container'
import { useRightPanelData } from '$/providers/rightPanel'
import DocumentationPanel from '@/components/DocumentationPanel.vue'
import { Err, Ok } from '@/util/data/result'
import { ResultComponent } from '@/util/react'
import { computed } from 'vue'

const container = useConainerData()
const rightPanel = useRightPanelData()
const displayedId = computed({
  get: () =>
    rightPanel.context?.help != null ?
      rightPanel.context.help.item
    : Err('Component help is available in Project View.'),
  set: (newSelection) => {
    rightPanel.updateContext(container.tab, (ctx) => {
      if (ctx.help == null) ctx.help = { item: newSelection, aiMode: false }
      else ctx.help.item = newSelection
      return ctx
    })
  },
})
</script>

<template>
  <DocumentationPanel
    v-if="displayedId?.ok"
    :selectedEntry="displayedId.value"
    :aiMode="rightPanel.context?.help?.aiMode ?? false"
    @update:selectedEntry="displayedId = Ok($event)"
  />
  <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
  <ResultComponent
    v-else-if="!displayedId.ok"
    status="info"
    :title="`${displayedId.error.payload}`"
    :centered="true"
  />
</template>

<style scoped>
.DocumentationPanel {
  --list-height: 0px;
  --radius-default: 20px;
  --background-color: #fff;
  --group-color-fallback: var(--color-dim);
}
</style>
