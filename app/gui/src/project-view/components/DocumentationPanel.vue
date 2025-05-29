<script setup lang="ts">
import Breadcrumbs, {
  type Item as Breadcrumb,
} from '@/components/DocumentationPanel/DocsBreadcrumbs.vue'
import DocsHeader from '@/components/DocumentationPanel/DocsHeader.vue'
import DocsList from '@/components/DocumentationPanel/DocsList.vue'
import { HistoryStack } from '@/components/DocumentationPanel/history'
import type { Docs, FunctionDocs, TypeDocs } from '@/components/DocumentationPanel/ir'
import { lookupDocumentation, placeholder } from '@/components/DocumentationPanel/ir'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import SvgButton from '@/components/SvgButton.vue'
import { groupColorStyle } from '@/composables/nodeColors'
import { useGraphStore } from '@/stores/graph'
import { injectProjectNames } from '@/stores/projectNames'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import { entryMethodPointer, suggestionDocumentationUrl } from '@/stores/suggestionDatabase/entry'
import { tryGetIndex } from '@/util/data/array'
import { type Opt } from '@/util/data/opt'
import { unwrapOr } from '@/util/data/result'
import type { Icon as IconName } from '@/util/iconMetadata/iconName'
import { ProjectPath } from '@/util/projectPath'
import { qnFromSegments, qnSegments, QualifiedName } from '@/util/qualifiedName'
import { computed, watch } from 'vue'
import FunctionSignatureEditor from './FunctionSignatureEditor.vue'
const props = defineProps<{ selectedEntry: SuggestionId | undefined; aiMode?: boolean }>()
const emit = defineEmits<{ 'update:selectedEntry': [value: SuggestionId | undefined] }>()
const db = useSuggestionDbStore()
const graph = useGraphStore(true)

const documentation = computed<Docs>(() => {
  if (props.aiMode)
    return placeholder('AI assistant mode: write query in natural language and press Enter.')
  const entry = props.selectedEntry
  return entry ? lookupDocumentation(db.entries, entry) : placeholder('No suggestion selected.')
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

const isPlaceholder = computed(() => documentation.value.kind === 'Placeholder')

const projectNames = injectProjectNames()

const name = computed<Opt<ProjectPath>>(() => {
  const docs = documentation.value
  return docs.kind === 'Placeholder' ? null : docs.name
})

// === Breadcrumbs ===

const suggestion = computed(() =>
  props.selectedEntry != null ? db.entries.get(props.selectedEntry) : undefined,
)

const color = computed(() => groupColorStyle(tryGetIndex(db.groups, suggestion.value?.groupIndex)))

const style = computed(() => ({
  '--enso-docs-group-color': color.value,
}))

const icon = computed<IconName>(() => suggestion.value?.iconName ?? 'marketplace')

const documentationUrl = computed(
  () => suggestion.value && suggestionDocumentationUrl(suggestion.value),
)

const methodPointer = computed(() => entryMethodPointer(suggestion.value))
const signatureAst = computed(() => {
  if (graph == null || methodPointer.value == null) return
  return unwrapOr(graph.getMethodAst(methodPointer.value), undefined)
})
const markdownDocs = computed(() => signatureAst.value?.mutableDocumentationMarkdown())

const historyStack = new HistoryStack()

// Reset breadcrumbs history when the user selects the entry from the component list.
watch(
  () => props.selectedEntry,
  (entry) => {
    if (entry && historyStack.current.value !== entry) {
      historyStack.reset(entry)
    }
  },
  { immediate: true },
)

// Update displayed documentation page when the user uses breadcrumbs.
watch(historyStack.current, (current) => {
  if (current) {
    emit('update:selectedEntry', current)
  }
})

const breadcrumbs = computed<Breadcrumb[]>(() => {
  if (name.value) {
    const segments = [...qnSegments(projectNames.printProjectPath(name.value))]
    return segments.slice(1).map((s) => ({ label: s.toLowerCase() }))
  } else {
    return []
  }
})

function handleBreadcrumbClick(index: number) {
  if (name.value) {
    const pathSegments = name.value.path ? qnSegments(name.value.path).slice(0, index) : []
    const path = pathSegments.length > 0 ? qnFromSegments(pathSegments) : ('Main' as QualifiedName)
    const projectPathSlice = name.value.withPath(path)
    const id = db.entries.findByProjectPath(projectPathSlice)
    if (id != null) {
      historyStack.record(id)
    }
  }
}

function openDocs(url: string) {
  window.open(url, '_blank')
}
</script>

<template>
  <div class="DocumentationPanel scrollable" :style="style" @wheel.stop.passive>
    <div v-if="!isPlaceholder" class="topBar">
      <Breadcrumbs
        :breadcrumbs="breadcrumbs"
        :color="color"
        :icon="icon"
        :canGoForward="historyStack.canGoForward()"
        :canGoBackward="historyStack.canGoBackward()"
        @click="(index) => handleBreadcrumbClick(index)"
        @forward="historyStack.forward()"
        @backward="historyStack.backward()"
      />
      <SvgButton
        v-if="documentationUrl"
        name="open"
        title="Open in New Window"
        @activate="openDocs(documentationUrl)"
      />
    </div>
    <FunctionSignatureEditor
      v-if="signatureAst"
      class="self-stretch"
      :functionAst="signatureAst"
      :methodPointer="methodPointer"
      :markdownDocs="markdownDocs"
    ></FunctionSignatureEditor>
    <h2 v-if="documentation.kind === 'Placeholder'">{{ documentation.text }}</h2>
    <template v-else>
      <div class="markdownDocs">
        <span v-if="documentation.documentation.length == 0">No documentation available.</span>
        <MarkdownEditor v-else :content="documentation.documentation" :toolbar="false" />
      </div>
      <div class="sections">
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
      </div>
    </template>
  </div>
</template>

<style scoped>
.DocumentationPanel {
  --enso-docs-type-name-color: #9640da;
  --enso-docs-methods-header-color: #1f71d3;
  --enso-docs-method-name-color: #1f71d3;
  --enso-docs-types-header-color: #1f71d3;
  --enso-docs-background-color: var(--background-color);
  --enso-docs-text-color: rbga(0, 0, 0, 0.6);
  font-family: var(--font-sans);
  font-size: 12px;
  line-height: 160%;
  color: var(--enso-docs-text-color);
  background-color: var(--enso-docs-background-color);
  padding: 4px 4px var(--doc-panel-bottom-clip, 0) 4px;
  white-space: normal;
  clip-path: inset(0 0 var(--doc-panel-bottom-clip, 0) 0);
  height: 100%;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 4px;
  align-items: flex-start;
}

.markdownDocs {
  margin: 4px 0 0 8px;
}

.tags {
  margin: 4px 0 0 8px;
}

.sections {
  width: 100%;
  padding: 0 8px;
}

.topBar {
  display: flex;
  width: 100%;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
}
</style>
