<script setup lang="ts">
import DocsExamples from '@/components/DocumentationPanel/DocsExamples.vue'
import DocsHeader from '@/components/DocumentationPanel/DocsHeader.vue'
import DocsList from '@/components/DocumentationPanel/DocsList.vue'
import DocsSynopsis from '@/components/DocumentationPanel/DocsSynopsis.vue'
import DocsTags from '@/components/DocumentationPanel/DocsTags.vue'
import type { Docs, FunctionDocs, Sections, TypeDocs } from '@/components/DocumentationPanel/ir'
import { lookupDocumentation, placeholder } from '@/components/DocumentationPanel/ir'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import { type Opt } from '@/util/opt'
import { computed } from 'vue'

const props = defineProps<{ selectedEntry: Opt<SuggestionId> }>()
const emit = defineEmits<{ 'update:selectedEntry': [id: SuggestionId] }>()
const db = useSuggestionDbStore()

const documentation = computed<Docs>(() => {
  const entry = props.selectedEntry
  return entry ? lookupDocumentation(db.entries, entry) : placeholder('No suggestion selected.')
})

const sections = computed<Sections>(() => {
  const docs: Docs = documentation.value
  const fallback = { tags: [], synopsis: [], examples: [] }
  return docs.kind === 'Placeholder' ? fallback : docs.sections
})

const methods = computed<FunctionDocs[]>(() => {
  const docs = documentation.value
  return docs.kind === 'Module' || docs.kind === 'Type' ? docs.methods : []
})

const constructors = computed<FunctionDocs[]>(() => {
  const docs = documentation.value
  return docs.kind === 'Type' ? docs.constructors : []
})

const types = computed<TypeDocs[]>(() => {
  const docs = documentation.value
  return docs.kind === 'Module' ? docs.types : []
})
</script>

<template>
  <div class="DocumentationPanel scrollable" @wheel.stop.passive>
    <h1 v-if="documentation.kind === 'Placeholder'">{{ documentation.text }}</h1>
    <DocsTags v-if="sections.tags.length > 0" :tags="sections.tags" />
    <div class="sections">
      <span v-if="sections.synopsis.length == 0">{{ 'No documentation available.' }}</span>
      <DocsSynopsis :sections="sections.synopsis" />
      <DocsHeader v-if="types.length > 0" kind="types" label="Types" />
      <DocsList
        :items="{ kind: 'Types', items: types }"
        @linkClicked="emit('update:selectedEntry', $event)"
      />
      <DocsHeader v-if="constructors.length > 0" kind="methods" label="Constructors" />
      <DocsList
        :items="{ kind: 'Constructors', items: constructors }"
        @linkClicked="emit('update:selectedEntry', $event)"
      />
      <DocsHeader v-if="methods.length > 0" kind="methods" label="Methods" />
      <DocsList
        :items="{ kind: 'Methods', items: methods }"
        @linkClicked="emit('update:selectedEntry', $event)"
      />
      <DocsHeader v-if="sections.examples.length > 0" kind="examples" label="Examples" />
      <DocsExamples :examples="sections.examples" />
    </div>
  </div>
</template>

<style scoped>
.DocumentationPanel {
  --enso-docs-type-name-color: #9640da;
  --enso-docs-methods-header-color: #1f71d3;
  --enso-docs-method-name-color: #1f71d3;
  --enso-docs-types-header-color: #1f71d3;
  --enso-docs-examples-header-color: #6da85e;
  --enso-docs-important-background-color: #edefe7;
  --enso-docs-info-background-color: #e6f1f8;
  --enso-docs-example-background-color: #e6f1f8;
  --enso-docs-background-color: #eaeaea;
  --enso-docs-text-color: rbga(0, 0, 0, 0.6);
  --enso-docs-tag-background-color: #dcd8d8;
  --enso-docs-code-background-color: #dddcde;
  font-family: 'M PLUS 1', DejaVuSansMonoBook, sans-serif;
  font-size: 11.5px;
  line-height: 160%;
  color: var(--enso-docs-text-color);
  background-color: var(--enso-docs-background-color);
  padding: 8px 12px 4px 8px;
  white-space: normal;
  clip-path: inset(0 0 4px 0);
  height: 100%;
  overflow-y: auto;
}

.sections {
  padding: 0 8px;
}
</style>
