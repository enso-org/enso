import type { SuggestionDb } from '@/stores/suggestionDatabase'
import type { SuggestionEntry, SuggestionId } from '@/stores/suggestionDatabase/entry'
import { SuggestionKind, entryQn } from '@/stores/suggestionDatabase/entry'
import type { Doc } from '@/util/docParser'
import type { QualifiedName } from '@/util/qualifiedName'
import type { SuggestionEntryArgument } from 'shared/languageServerTypes/suggestions'

// === Types ===

/**
 * Intermediate representation of the entries documentation.
 */

export type Docs = FunctionDocs | TypeDocs | ModuleDocs | LocalDocs | Placeholder

export interface Placeholder {
  kind: 'Placeholder'
  text: string
}

export interface FunctionDocs {
  kind: 'Function'
  id: SuggestionId
  name: QualifiedName
  arguments: SuggestionEntryArgument[]
  sections: Sections
}

export interface TypeDocs {
  kind: 'Type'
  id: SuggestionId
  name: QualifiedName
  arguments: SuggestionEntryArgument[]
  sections: Sections
  methods: FunctionDocs[]
  constructors: FunctionDocs[]
}

export interface ModuleDocs {
  kind: 'Module'
  id: SuggestionId
  name: QualifiedName
  sections: Sections
  types: TypeDocs[]
  methods: FunctionDocs[]
}

export interface LocalDocs {
  kind: 'Local'
  id: SuggestionId
  name: QualifiedName
  sections: Sections
}

// === Sections ===

export interface Example {
  header?: string
  body: Doc.HtmlString
}

export function placeholder(text: string): Placeholder {
  return { kind: 'Placeholder', text }
}

/**
 * Documentation sections split into three categories.
 *
 * These categories of sections are present in almost every documentation page.
 */
export interface Sections {
  tags: Doc.Section.Tag[]
  synopsis: Doc.Section[]
  examples: Example[]
}

// Split doc sections into categories.
function filterSections(sections: Iterable<Doc.Section>): Sections {
  const tags = []
  const synopsis = []
  const examples = []

  for (const section of sections) {
    const isTag = 'Tag' in section
    const isExample = 'Marked' in section && section.Marked.mark === 'Example'
    if (isTag) {
      tags.push(section.Tag)
    } else if (isExample) {
      examples.push({ ...section.Marked } as Example)
    } else {
      synopsis.push(section)
    }
  }
  return { tags, synopsis, examples }
}

// === Lookup ===

export function lookupDocumentation(db: SuggestionDb, id: SuggestionId): Docs {
  const entry = db.get(id)
  if (!entry)
    return placeholder(
      `Documentation not available. Entry with id ${id} not found in the database.`,
    )
  const handle = handleDocumentation[entry.kind]
  return handle ? handle(db, entry, id) : placeholder(`Entry kind ${entry.kind} was not handled.`)
}

function getChildren(db: SuggestionDb, id: SuggestionId, kind: SuggestionKind): Docs[] {
  if (!id) return []
  const children = Array.from(db.childIdToParentId.reverseLookup(id))
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
    if (doc.kind === 'Function') {
      return [doc]
    } else {
      console.error(`Unexpected docs type: ${docs}, expected Function`)
      return []
    }
  })
}

function asTypeDocs(docs: Docs[]): TypeDocs[] {
  return docs.flatMap((doc) => {
    if (doc.kind === 'Type') {
      return [doc]
    } else {
      console.error(`Unexpected docs type: ${docs}, expected Type`)
      return []
    }
  })
}

type DocsHandle = (db: SuggestionDb, entry: SuggestionEntry, id: SuggestionId) => Docs

const handleFunction: DocsHandle = (_db, entry, id) => ({
  kind: 'Function',
  id,
  name: entryQn(entry),
  arguments: entry.arguments,
  sections: filterSections(entry.documentation),
})

const handleDocumentation: Record<SuggestionKind, DocsHandle> = {
  [SuggestionKind.Function]: handleFunction,
  [SuggestionKind.Method]: handleFunction,
  [SuggestionKind.Constructor]: handleFunction,
  [SuggestionKind.Local]: (_db, entry, id) => ({
    kind: 'Local',
    id,
    name: entryQn(entry),
    sections: filterSections(entry.documentation),
  }),
  [SuggestionKind.Type]: (db, entry, id) => ({
    kind: 'Type',
    id,
    name: entryQn(entry),
    arguments: entry.arguments,
    sections: filterSections(entry.documentation),
    methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
    constructors: asFunctionDocs(getChildren(db, id, SuggestionKind.Constructor)),
  }),
  [SuggestionKind.Module]: (db, entry, id) => ({
    kind: 'Module',
    id,
    name: entryQn(entry),
    sections: filterSections(entry.documentation),
    types: asTypeDocs(getChildren(db, id, SuggestionKind.Type)),
    methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
  }),
}
