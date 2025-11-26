<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { SuggestionId } from '$/providers/openedProjects/suggestionDatabase/entry'
import { suggestionDocumentationUrl } from '$/providers/openedProjects/suggestionDatabase/entry'
import Breadcrumbs, {
  type Item as Breadcrumb,
} from '@/components/ComponentHelp/DocsBreadcrumbs.vue'
import DocsHeader from '@/components/ComponentHelp/DocsHeader.vue'
import DocsList from '@/components/ComponentHelp/DocsList.vue'
import { HistoryStack } from '@/components/ComponentHelp/history'
import type { Docs, FunctionDocs, TypeDocs } from '@/components/ComponentHelp/ir'
import { lookupDocumentation, placeholder } from '@/components/ComponentHelp/ir'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import SvgButton from '@/components/SvgButton.vue'
import { groupColorStyle } from '@/composables/nodeColors'
import { tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import type { Icon as IconName } from '@/util/iconMetadata/iconName'
import { ProjectPath } from '@/util/projectPath'
import { qnFromSegments, qnSegments, type QualifiedName } from '@/util/qualifiedName'
import { EditorView } from '@codemirror/view'
import { computed, watch } from 'vue'

const props = defineProps<{ selectedEntry: SuggestionId | undefined; aiMode?: boolean }>()
const emit = defineEmits<{ 'update:selectedEntry': [value: SuggestionId | undefined] }>()

const { suggestionDb: db, projectNames } = useCurrentProject()

const documentation = computed<Docs>(() => {
  if (props.aiMode)
    return placeholder('AI assistant mode: write query in natural language and press Enter.')
  const entry = props.selectedEntry
  return entry ?
      lookupDocumentation(db.value.entries, entry)
    : placeholder('No suggestion selected.')
})

function syncMarkdownDocumentation(view: EditorView) {
  watch(
    documentation,
    (docs) => {
      if (docs.kind !== 'Placeholder') {
        view.dispatch({
          changes: { from: 0, to: view.state.doc.length, insert: docs.documentation },
        })
      }
    },
    { immediate: true },
  )
}

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

const name = computed<Opt<ProjectPath>>(() => {
  const docs = documentation.value
  return docs.kind === 'Placeholder' ? null : docs.name
})

// === Breadcrumbs ===

const suggestion = computed(() =>
  props.selectedEntry != null ? db.value.entries.get(props.selectedEntry) : undefined,
)

const color = computed(() =>
  groupColorStyle(tryGetIndex(db.value.groups, suggestion.value?.groupIndex)),
)

const style = computed(() => ({
  '--enso-docs-group-color': color.value,
}))

const icon = computed<IconName>(() => suggestion.value?.iconName ?? 'marketplace')

const documentationUrl = computed(
  () => suggestion.value && suggestionDocumentationUrl(suggestion.value),
)

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
    const segments = [...qnSegments(projectNames.value.printProjectPath(name.value))]
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
    const id = db.value?.entries.findByProjectPath(projectPathSlice)
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
  <div class="ComponentHelp scrollable" :style="style" @wheel.stop.passive>
    <div v-if="!isPlaceholder" class="topBar">
      <div class="breadcrumbsWrapper">
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
      </div>
      <SvgButton
        v-if="documentationUrl"
        name="open"
        title="Open in New Window"
        @activate="openDocs(documentationUrl)"
      />
    </div>
    <h2 v-if="documentation.kind === 'Placeholder'">{{ documentation.text }}</h2>
    <template v-else>
      <div class="markdownDocs">
        <span v-if="documentation.documentation.length == 0">No documentation available.</span>
        <MarkdownEditor
          v-else
          :content="documentation.documentation"
          :toolbar="false"
          :readonly="true"
          :editorReadyCallback="syncMarkdownDocumentation"
        />
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
.ComponentHelp {
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
  gap: 8px;
}

.breadcrumbsWrapper {
  flex: 0 1 auto;
  min-width: 0;
  overflow: hidden;
}

.topBar .SvgButton {
  flex: 0 0 auto;
}
</style>
