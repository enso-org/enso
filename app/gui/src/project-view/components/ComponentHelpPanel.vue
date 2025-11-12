<script setup lang="ts">
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { useContainerData } from '$/providers/container'
import { useRightPanelData } from '$/providers/rightPanel'
import ComponentHelp from '@/components/ComponentHelp.vue'
import { ResultComponent } from '@/util/react'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { computed } from 'vue'

const UNAVAILABLE_MESSAGE = 'Component help is available in Project View.'

const container = useContainerData()
const rightPanel = useRightPanelData()
const displayedId = computed({
  get: () =>
    rightPanel.context?.help != null ? rightPanel.context.help.item : Err(UNAVAILABLE_MESSAGE),
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
  <WithCurrentProject :id="rightPanel.focusedProject">
    <ComponentHelp
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
    <template #fallback>
      <ResultComponent status="info" :title="UNAVAILABLE_MESSAGE" :centered="true" />
    </template>
  </WithCurrentProject>
</template>

<style scoped>
.ComponentHelp {
  --list-height: 0px;
  --radius-default: 20px;
  --background-color: #fff;
  --group-color-fallback: var(--color-dim);
}
</style>
