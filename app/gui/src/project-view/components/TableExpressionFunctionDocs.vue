<script setup lang="ts">
import type { MethodSuggestionEntry } from '$/providers/openedProjects/suggestionDatabase/entry'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { useStringSync } from '@/util/codemirror'
import { computed } from 'vue'

const { documentation } = defineProps<{
  documentation: MethodSuggestionEntry
}>()

const name = computed(() => documentation.name)
const args = computed(() =>
  documentation.arguments
    .filter((arg) => !arg.hasDefault)
    .map((arg) => (arg.name === 'self' ? '[column]' : arg.name)),
)
const summary = computed(() =>
  (documentation.documentationSummary ?? '').replaceAll('`self`', '`column`'),
)
const { syncExt, setText } = useStringSync()
</script>

<template>
  <div class="TableExpressionFunctionDocs">
    <div class="usage">{{ name }}({{ args.join(', ') }})</div>
    <MarkdownEditor
      :extensions="syncExt"
      :toolbar="false"
      :editorReadyCallback="(view) => setText(view, summary)"
    />
  </div>
</template>

<style scoped>
.TableExpressionFunctionDocs {
  padding: 1px 4px;
}

.usage {
  font-family: var(--font-mono);
}
</style>
