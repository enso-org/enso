<script setup lang="ts">
import { default as DocsExamples } from '@/components/documentation/DocsExamples.vue'
import { default as DocsHeader } from '@/components/documentation/DocsHeader.vue'
import { default as DocsList } from '@/components/documentation/DocsList.vue'
import { default as DocsSynopsis } from '@/components/documentation/DocsSynopsis.vue'
import { default as DocsTags } from '@/components/documentation/DocsTags.vue'
import { useSuggestionDbStore, type SuggestionDb } from '@/stores/suggestionDatabase'
import type { SuggestionEntry, SuggestionId } from '@/stores/suggestionDatabase/entry'
import { SuggestionKind } from '@/stores/suggestionDatabase/entry'
import type { Doc } from '@/util/docParser'
import { type Opt } from '@/util/opt'
import type { SuggestionEntryArgument } from 'shared/languageServerTypes/suggestions'
import { computed } from 'vue'

const props = defineProps<{ selectedEntry: Opt<SuggestionId> }>()
const emit = defineEmits<{ 'update:selectedEntry': [id: SuggestionId] }>()

// === Types ===

/**
 * Intermediate representation of the entries documentation.
 */
export type Docs =
  | { Function: FunctionDocs }
  | { Type: TypeDocs }
  | { Module: ModuleDocs }
  | { Local: LocalDocs }
  | { Placeholder: string }

export interface FunctionDocs {
  id: SuggestionId
  name: string
  arguments: SuggestionEntryArgument[]
  sections: Sections
}

export interface TypeDocs {
  id: SuggestionId
  name: string
  arguments: SuggestionEntryArgument[]
  sections: Sections
  methods: FunctionDocs[]
  constructors: FunctionDocs[]
}

export interface ModuleDocs {
  id: SuggestionId
  name: string
  sections: Sections
  types: TypeDocs[]
  methods: FunctionDocs[]
}

export interface LocalDocs {
  id: SuggestionId
  name: string
  sections: Sections
}

// === Sections ===

/**
 * Documentation sections split into three categories. These categories of sections are present in almost every documentation page.
 */
export interface Sections {
  tags: Doc.Section.Tag[]
  synopsis: Doc.Section[]
  examples: Doc.Section.Marked[]
}

// Split doc sections into categories.
function filterSections(sections: Iterable<Doc.Section>): Sections {
  const tags = []
  const synopsis = []
  const examples = []

  for (const section of sections) {
    const isTag = 'Tag' in section
    const isExample = 'Marked' in section && section.Marked.mark == 'Example'
    if (isTag) {
      tags.push(section.Tag)
    } else if (isExample) {
      examples.push(section.Marked)
    } else {
      synopsis.push(section)
    }
  }
  return { tags, synopsis, examples }
}

// === Lookup ===

function getChildren(db: SuggestionDb, id: SuggestionId, kind: SuggestionKind): Docs[] {
  if (!id) return []
  const children = Array.from(db.parent.reverseLookup(id))
  return children.reduce((acc: Docs[], id: SuggestionId) => {
    if (db.get(id)?.kind === kind) {
      const docs = lookupDocumentation(db, id)
      acc.push(docs)
    }
    return acc
  }, [])
}

function asFunctionDocs(docs: Docs[]): FunctionDocs[] {
  return docs.flatMap((doc) => {
    if ('Function' in doc) {
      return [doc.Function]
    } else {
      console.error(`Unexpected docs type: ${docs}, expected Function`)
      return []
    }
  })
}

function asTypeDocs(docs: Docs[]): TypeDocs[] {
  return docs.flatMap((doc) => {
    if ('Type' in doc) {
      return [doc.Type]
    } else {
      console.error(`Unexpected docs type: ${docs}, expected Type`)
      return []
    }
  })
}

type DocsHandle = (db: SuggestionDb, entry: SuggestionEntry, id: SuggestionId) => Docs

const handleFunction: DocsHandle = (_db, entry, id) => ({
  Function: {
    name: entry.name,
    id,
    arguments: entry.arguments,
    sections: filterSections(entry.documentation),
  },
})

const handleDocumentation: Record<SuggestionKind, DocsHandle> = {
  [SuggestionKind.Function]: handleFunction,
  [SuggestionKind.Method]: handleFunction,
  [SuggestionKind.Constructor]: handleFunction,
  [SuggestionKind.Local]: (_db, entry, id) => ({
    Local: {
      name: entry.name,
      id,
      sections: filterSections(entry.documentation),
    },
  }),
  [SuggestionKind.Type]: (db, entry, id) => {
    return {
      Type: {
        name: entry.name,
        id,
        arguments: entry.arguments,
        sections: filterSections(entry.documentation),
        methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
        constructors: asFunctionDocs(getChildren(db, id, SuggestionKind.Constructor)),
      },
    }
  },
  [SuggestionKind.Module]: (db, entry, id) => {
    return {
      Module: {
        name: entry.name,
        id,
        sections: filterSections(entry.documentation),
        types: asTypeDocs(getChildren(db, id, SuggestionKind.Type)),
        methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
      },
    }
  },
}

function lookupDocumentation(db: SuggestionDb, id: SuggestionId): Docs {
  const entry = db.get(id)
  if (!entry) return { Placeholder: 'Entry not found' }
  const handle = handleDocumentation[entry.kind]
  return handle ? handle(db, entry, id) : { Placeholder: `Entry kind not handled: ${entry.kind}` }
}

// === Helper variables ===

const documentation = computed<Docs>(() => {
  const db = useSuggestionDbStore()
  const entry = props.selectedEntry
  return entry ? lookupDocumentation(db.entries, entry) : { Placeholder: 'No entry selecter' }
})

const sections = computed<Sections>(() => {
  const docs: Docs = documentation.value
  const fallback = { tags: [], synopsis: [], examples: [] }
  if ('Function' in docs) {
    return docs.Function.sections
  } else if ('Type' in docs) {
    return docs.Type.sections
  } else if ('Module' in docs) {
    return docs.Module.sections
  } else if ('Local' in docs) {
    return docs.Local.sections
  } else {
    return fallback
  }
})
const methods = computed<FunctionDocs[]>(() => {
  const docs = documentation.value
  if ('Module' in docs) {
    return docs.Module.methods
  } else if ('Type' in docs) {
    return docs.Type.methods
  } else {
    return []
  }
})
const constructors = computed<FunctionDocs[]>(() => {
  const docs = documentation.value
  if ('Type' in docs) {
    return docs.Type.constructors
  } else {
    return []
  }
})
const types = computed<TypeDocs[]>(() => {
  const docs = documentation.value
  if ('Module' in docs) {
    return docs.Module.types
  } else {
    return []
  }
})
</script>

<template>
  <div class="DocumentationPanel">
    <h1 v-if="'Placeholder' in documentation">{{ documentation.Placeholder }}</h1>
    <DocsTags v-if="sections.tags.length > 0" :tags="sections.tags" />
    <DocsSynopsis :sections="sections.synopsis" />
    <DocsHeader v-if="types.length > 0" kind="types" label="Types" />
    <DocsList :items="{ Types: types }" @linkClicked="emit('update:selectedEntry', $event)" />
    <DocsHeader v-if="constructors.length > 0" kind="methods" label="Constructors" />
    <DocsList
      :items="{ Constructors: constructors }"
      @linkClicked="emit('update:selectedEntry', $event)"
    />
    <DocsHeader v-if="methods.length > 0" kind="methods" label="Methods" />
    <DocsList :items="{ Methods: methods }" @linkClicked="emit('update:selectedEntry', $event)" />
    <DocsHeader v-if="sections.examples.length > 0" kind="examples" label="Examples" />
    <DocsExamples :examples="sections.examples" />
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
  padding: 8px 8px 4px;
  white-space: normal;
}

:deep(.sectionContent) {
  padding: 0 8px;
}
</style>
