<script setup lang="ts">
import { injectConainerData } from '$/providers/container'
import DocumentationPanel from '@/components/DocumentationPanel.vue'
import { Ok } from '@/util/data/result'
import { computed } from 'vue'

const { tab, rightPanel } = injectConainerData()
const displayedId = computed({
  get: () => (rightPanel.context?.help != null ? rightPanel.context.help.item : Ok(undefined)),
  set: (newSelection) => {
    rightPanel.updateContext(tab.value, (ctx) => {
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
  <div v-else-if="!displayedId.ok" class="help-placeholder">{{ displayedId.error.payload }}.</div>
</template>

<style scoped>
.DocumentationPanel {
  --list-height: 0px;
  --radius-default: 20px;
  --background-color: #fff;
  --group-color-fallback: var(--color-dim);
}

.help-placeholder {
  height: 100%;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
}
</style>
