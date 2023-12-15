<script setup lang="ts">
import type { Item as Breadcrumb } from '@/components/DocumentationPanel/DocsBreadcrumbs.vue'
import Breadcrumbs from '@/components/DocumentationPanel/DocsBreadcrumbs.vue'
import DocsExamples from '@/components/DocumentationPanel/DocsExamples.vue'
import DocsHeader from '@/components/DocumentationPanel/DocsHeader.vue'
import DocsList from '@/components/DocumentationPanel/DocsList.vue'
import DocsSynopsis from '@/components/DocumentationPanel/DocsSynopsis.vue'
import DocsTags from '@/components/DocumentationPanel/DocsTags.vue'
import { HistoryStack } from '@/components/DocumentationPanel/history'
import type { Docs, FunctionDocs, Sections, TypeDocs } from '@/components/DocumentationPanel/ir'
import { lookupDocumentation, placeholder } from '@/components/DocumentationPanel/ir'
import { groupColorStyle, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import { tryGetIndex } from '@/util/data/array'
import { type Opt } from '@/util/data/opt'
import type { Icon as IconName } from '@/util/iconName'
import type { QualifiedName } from '@/util/qualifiedName'
import { qnSegments, qnSlice } from '@/util/qualifiedName'
import { computed, watch } from 'vue'

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

const isPlaceholder = computed(() => 'Placeholder' in documentation.value)

const name = computed<Opt<QualifiedName>>(() => {
  const docs = documentation.value
  return docs.kind === 'Placeholder' ? null : docs.name
})

// === Breadcrumbs ===

const color = computed(() => {
  const groupIndex =
    props.selectedEntry != null ? db.entries.get(props.selectedEntry)?.groupIndex : undefined
  return groupColorStyle(tryGetIndex(db.groups, groupIndex))
})

const icon = computed<IconName>(() => {
  const id = props.selectedEntry
  if (id) {
    const entry = db.entries.get(id)
    return entry?.iconName ?? 'marketplace'
  } else {
    return 'marketplace'
  }
})

const historyStack = new HistoryStack()

// Reset breadcrumbs history when the user selects the entry from the component list.
watch(
  () => props.selectedEntry,
  (entry) => {
    if (entry && historyStack.current.value !== entry) {
      historyStack.reset(entry)
    }
  },
)

// Update displayed documentation page when the user uses breadcrumbs.
watch(historyStack.current, (current) => {
  if (current) {
    emit('update:selectedEntry', current)
  }
})

const breadcrumbs = computed<Breadcrumb[]>(() => {
  if (name.value) {
    const segments = qnSegments(name.value)
    return segments.slice(1).map((s) => ({ label: s.toLowerCase() }))
  } else {
    return []
  }
})

function handleBreadcrumbClick(index: number) {
  if (name.value) {
    const qName = qnSlice(name.value, 0, index + 2)
    if (qName.ok) {
      const [id] = db.entries.nameToId.lookup(qName.value)
      if (id) {
        historyStack.record(id)
      }
    }
  }
}
</script>

<template>
  <div class="DocumentationPanel scrollable" @wheel.stop.passive>
    <h1 v-if="documentation.kind === 'Placeholder'">{{ documentation.text }}</h1>
    <Breadcrumbs
      v-if="!isPlaceholder"
      :breadcrumbs="breadcrumbs"
      :color="color"
      :icon="icon"
      :canGoForward="historyStack.canGoForward()"
      :canGoBackward="historyStack.canGoBackward()"
      @click="(index) => handleBreadcrumbClick(index)"
      @forward="historyStack.forward()"
      @backward="historyStack.backward()"
    />
    <DocsTags v-if="sections.tags.length > 0" class="tags" :tags="sections.tags" />
    <div class="sections">
      <span v-if="sections.synopsis.length == 0">{{ 'No documentation available.' }}</span>
      <DocsSynopsis :sections="sections.synopsis" />
      <DocsHeader v-if="types.length > 0" kind="types" label="Types" />
      <DocsList
        :items="{ kind: 'Types', items: types }"
        @linkClicked="historyStack.record($event)"
      />
      <DocsHeader v-if="constructors.length > 0" kind="methods" label="Constructors" />
      <DocsList
        :items="{ kind: 'Constructors', items: constructors }"
        @linkClicked="historyStack.record($event)"
      />
      <DocsHeader v-if="methods.length > 0" kind="methods" label="Methods" />
      <DocsList
        :items="{ kind: 'Methods', items: methods }"
        @linkClicked="historyStack.record($event)"
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
  font-family: var(--font-sans);
  font-size: 11.5px;
  line-height: 160%;
  color: var(--enso-docs-text-color);
  background-color: var(--enso-docs-background-color);
  padding: 4px 12px 4px 4px;
  white-space: normal;
  clip-path: inset(0 0 4px 0);
  height: 100%;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
}

.tags {
  margin: 4px 0 0 8px;
}

.sections {
  padding: 0 8px;
}
</style>
