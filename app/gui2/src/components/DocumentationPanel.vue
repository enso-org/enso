<script setup lang="ts">
 import { default as SvgIcon } from '@/components/SvgIcon.vue'
 import { default as List } from '@/components/documentation/List.vue'
 import { default as Synopsis } from '@/components/documentation/Synopsis.vue'
 import { makeMethod } from '@/stores/suggestionDatabase/entry'
 import type { Doc } from '@/util/docParser'
 import type { SuggestionEntryArgument } from 'shared/languageServerTypes/suggestions'
 import { computed, ref } from 'vue'
 import { type SuggestionDb, useSuggestionDbStore } from '@/stores/suggestionDatabase'
 import type { SuggestionId, SuggestionEntry } from '@/stores/suggestionDatabase/entry'
 import { type Opt } from '@/util/opt'
 import { SuggestionKind, entryQn } from '@/stores/suggestionDatabase/entry'
 import { default as Header, type Kind as HeaderKind } from '@/components/documentation/Header.vue'
 import { default as Tags } from '@/components/documentation/Tags.vue'
 import { default as Examples } from '@/components/documentation/Examples.vue'

 const props = defineProps<{ selectedEntry: Opt<SuggestionEntry> }>()

 
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
   name: string
   arguments: SuggestionEntryArgument[]
   sections: Sections
 }

 export interface TypeDocs {
   name: string
   arguments: SuggestionEntryArgument[]
   sections: Sections
   methods: FunctionDocs[]
   constructors: FunctionDocs[]
 }

 export interface ModuleDocs {
   name: string
   sections: Sections
   types: TypeDocs[]
   methods: FunctionDocs[]
 }

 export interface LocalDocs {
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

 function getFunctionChildren(db: SuggestionDb, id: SuggestionId, kind: SuggestionKind): FunctionDocs[] {
   if (!id) return []
   const children = Array.from(db.parent.reverseLookup(id)).map(id => db.get(id))
   return Array.from(children
     .filter(child => child && child.kind === kind)
     .flatMap(child => {
       if (!child) return []
       const docs = lookupDocumentation(db, child)
       if ('Function' in docs) {
         return docs.Function
       } else {
         return []
       }
   }))
 }
 
 function getTypeChildren(db: SuggestionDb, id: SuggestionId, kind: SuggestionKind): TypeDocs[] {
   if (!id) return []
   const children = Array.from(db.parent.reverseLookup(id)).map(id => db.get(id))
   return Array.from(children
     .filter(child => child && child.kind === kind)
     .flatMap(child => {
       if (!child) return []
       const docs = lookupDocumentation(db, child)
       if ('Type' in docs) {
         return docs.Type
       } else {
         return []
       }
   }))
 }

 type DocsHandle = (db: SuggestionDb, entry: SuggestionEntry) => Docs
 
 const handleFunction: DocsHandle = (db, entry) => ({ Function: {
   name: entry.name,
   arguments: entry.arguments,
   sections: filterSections(entry.documentation),
 }})

 const handleDocumentation: Record<SuggestionKind, DocsHandle> = {
   [SuggestionKind.Function]: handleFunction,
   [SuggestionKind.Method]: handleFunction,
   [SuggestionKind.Constructor]: handleFunction,
   [SuggestionKind.Local]: (db, entry) => ({ Local: {
     name: entry.name,
     sections: filterSections(entry.documentation),
   }}),
   [SuggestionKind.Type]: (db, entry) => {
     const [entryId] = db.nameToId.lookup(entryQn(entry))
     if (!entryId) return { Placeholder: 'No documentation available' }
     return { Type: {
       name: entry.name,
       arguments: entry.arguments,
       sections: filterSections(entry.documentation),
       methods: getFunctionChildren(db, entryId, SuggestionKind.Method),
       constructors: getFunctionChildren(db, entryId, SuggestionKind.Constructor),
     }}
   },
   [SuggestionKind.Module]: (db, entry) => {
     const [entryId] = db.nameToId.lookup(entryQn(entry))
     if (!entryId) return { Placeholder: 'No documentation available' }
     return { Module: {
       name: entry.name,
       sections: filterSections(entry.documentation),
       types: getTypeChildren(db, entryId, SuggestionKind.Type),
       methods: getFunctionChildren(db, entryId, SuggestionKind.Method),
     }}
   },
 }
 
 function lookupDocumentation(db: SuggestionDb, entry: SuggestionEntry): Docs {
   const handle = handleDocumentation[entry.kind]
   return handle ? handle(db, entry) : { Placeholder: `Entry kind not handled: ${entry.kind}` }
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
    <Tags v-if="sections.tags.length > 0" :tags="sections.tags" />
    <Synopsis :sections="sections.synopsis" />
    <Header v-if="types.length > 0" kind="types" label="Types" />
    <List :items="{ Types: types }" />
    <Header v-if="constructors.length > 0" kind="methods" label="Constructors" />
    <List :items="{ Constructors: constructors }" />
    <Header v-if="methods.length > 0" kind="methods" label="Methods" />
    <List :items="{ Methods: methods }" />
    <Header v-if="sections.examples.length > 0" kind="examples" label="Examples" />
    <Examples :examples="sections.examples" />
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
