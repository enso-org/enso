<script setup lang="ts">
import DocumentationPanel from '@/components/DocumentationPanel.vue'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { computed, watch } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { Err, Ok, unwrapOr } from 'ydoc-shared/util/data/result'

// A displayed component can be overridren by this model, e.g. when the user clicks links in the documenation.
const overrideDisplayed = defineModel<SuggestionId | undefined>()
const props = defineProps<{ aiMode?: boolean }>()

const selection = injectGraphSelection()
const graphStore = useGraphStore()

function docsForSelection() {
  const selected = selection.tryGetSoleSelection()
  if (!selected.ok) return Err('Select a single component to display help')
  const suggestionId = graphStore.db.nodeMainSuggestionId.lookup(selected.value)
  if (suggestionId == null) return Err('No documentation available for selected component')
  return Ok(suggestionId)
}

const docs = computed(() => docsForSelection())
// When the selection changes, we cancel the displayed suggestion override that can be in place.
watch(docs, (_) => (overrideDisplayed.value = undefined))

const displayedId = computed(() => overrideDisplayed.value ?? unwrapOr(docs.value, null))
</script>

<template>
  <DocumentationPanel
    v-if="displayedId"
    :selectedEntry="displayedId"
    :aiMode="props.aiMode"
    @update:selectedEntry="overrideDisplayed = $event"
  />
  <div v-else-if="!displayedId && !docs.ok" class="help-placeholder">{{ docs.error.payload }}.</div>
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
