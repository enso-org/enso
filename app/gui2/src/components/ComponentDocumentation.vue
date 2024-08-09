<script setup lang="ts">
import DocumentationPanel from '@/components/DocumentationPanel.vue'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { ref, watchEffect } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { Err, Ok, type Result } from 'ydoc-shared/util/data/result'

const selection = injectGraphSelection()
const graphStore = useGraphStore()

const displayedDocs = ref<Result<SuggestionId>>()

function docsForSelection() {
  const selected = selection.tryGetSoleSelection()
  if (!selected.ok) return Err('Select a single component to display help')
  const suggestionId = graphStore.db.nodeMainSuggestionId.lookup(selected.value)
  if (suggestionId == null) return Err('No documentation available for selected component')
  return Ok(suggestionId)
}
watchEffect(() => (displayedDocs.value = docsForSelection()))
</script>

<template>
  <DocumentationPanel
    v-if="displayedDocs?.ok"
    :selectedEntry="displayedDocs.value"
    @update:selectedEntry="displayedDocs = Ok($event)"
  />
  <div v-else-if="displayedDocs?.ok === false" class="help-placeholder">
    {{ displayedDocs.error.payload }}.
  </div>
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
