<script setup lang="ts">
import DocumentationPanel from '@/components/DocumentationPanel.vue'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { computed } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { Err, Ok } from 'ydoc-shared/util/data/result'

const props = defineProps<{ displayedSuggestionId: SuggestionId | null }>()
const emit = defineEmits<{ 'update:displayedSuggestionId': [SuggestionId] }>()
const selection = injectGraphSelection()
const graphStore = useGraphStore()

function docsForSelection() {
  const selected = selection.tryGetSoleSelection()
  if (!selected.ok) return Err('Select a single component to display help')
  const suggestionId = graphStore.db.nodeMainSuggestionId.lookup(selected.value)
  if (suggestionId == null) return Err('No documentation available for selected component')
  return Ok(suggestionId)
}

const displayedId = computed(() =>
  props.displayedSuggestionId != null ? Ok(props.displayedSuggestionId) : docsForSelection(),
)
</script>

<template>
  <DocumentationPanel
    v-if="displayedId?.ok"
    :selectedEntry="displayedId.value"
    @update:selectedEntry="emit('update:displayedSuggestionId', $event)"
  />
  <div v-else-if="displayedId?.ok === false" class="help-placeholder">
    {{ displayedId.error.payload }}.
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
