import type { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import type {
  SuggestionEntry,
  SuggestionId,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { SuggestionKind } from '$/providers/openedProjects/suggestionDatabase/entry'
import { type ProjectPath } from '@/util/projectPath'
import * as iter from 'enso-common/src/utilities/data/iter'
import type { SuggestionEntryArgument } from 'ydoc-shared/languageServerTypes/suggestions'

// === Types ===

/** Intermediate representation of the entries documentation. */

export type Docs = FunctionDocs | TypeDocs | ModuleDocs | LocalDocs | Placeholder

export interface Placeholder {
  kind: 'Placeholder'
  text: string
}

export interface FunctionDocs {
  kind: 'Function'
  id: SuggestionId
  name: ProjectPath
  arguments: SuggestionEntryArgument[]
  documentation: string
  documentationSummary: string | undefined
}

export interface TypeDocs {
  kind: 'Type'
  id: SuggestionId
  name: ProjectPath
  arguments: SuggestionEntryArgument[]
  documentation: string
  documentationSummary: string | undefined
  methods: FunctionDocs[]
  constructors: FunctionDocs[]
}

export interface ModuleDocs {
  kind: 'Module'
  id: SuggestionId
  name: ProjectPath
  documentation: string
  documentationSummary: string | undefined
  types: TypeDocs[]
  methods: FunctionDocs[]
}

export interface LocalDocs {
  kind: 'Local'
  id: SuggestionId
  name: ProjectPath
  documentation: string
  documentationSummary: string | undefined
}

/** Placeholder constructor. */
export function placeholder(text: string): Placeholder {
  return { kind: 'Placeholder', text }
}

// === Lookup ===

/** The main function for getting documentation page for given entry. */
export function lookupDocumentation(db: SuggestionDb, id: SuggestionId): Docs {
  const entry = db.get(id)
  if (!entry)
    return placeholder(
      `Documentation not available. Entry with id ${id} not found in the database.`,
    )
  const handle = handleDocumentation[entry.kind]
  if (!handle) return placeholder(`Entry kind ${entry.kind} was not handled.`)
  return handle(db, entry as any, id)
}

function getChildren(db: SuggestionDb, id: SuggestionId, kind: SuggestionKind): Docs[] {
  if (!id) return []
  const children = db.childIdToParentId.reverseLookup(id)
  return [
    ...iter.filterDefined(
      iter.map(children, (id: SuggestionId) => {
        const entry = db.get(id)
        return entry?.kind === kind && !entry?.isPrivate ? lookupDocumentation(db, id) : undefined
      }),
    ),
  ]
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

type DocsHandler<Kind extends SuggestionKind> = (
  db: SuggestionDb,
  entry: SuggestionEntry & { kind: Kind },
  id: SuggestionId,
) => Docs

const handleFunction: DocsHandler<
  SuggestionKind.Function | SuggestionKind.Method | SuggestionKind.Constructor
> = (_db, entry, id) => ({
  kind: 'Function',
  id,
  name: entry.definitionPath,
  arguments: entry.arguments,
  documentation: entry.documentation,
  documentationSummary: entry.documentationSummary,
})

const handleDocumentation: { [Kind in SuggestionKind]: DocsHandler<Kind> } = {
  [SuggestionKind.Function]: handleFunction,
  [SuggestionKind.Method]: handleFunction,
  [SuggestionKind.Constructor]: handleFunction,
  [SuggestionKind.Local]: (_db, entry, id) => ({
    kind: 'Local',
    id,
    name: entry.definitionPath,
    documentation: entry.documentation,
    documentationSummary: entry.documentationSummary,
  }),
  [SuggestionKind.Type]: (db, entry, id) => ({
    kind: 'Type',
    id,
    name: entry.definitionPath,
    arguments: entry.arguments,
    documentation: entry.documentation,
    documentationSummary: entry.documentationSummary,
    methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
    constructors: asFunctionDocs(getChildren(db, id, SuggestionKind.Constructor)),
  }),
  [SuggestionKind.Module]: (db, entry, id) => ({
    kind: 'Module',
    id,
    name: entry.definitionPath,
    documentation: entry.documentation,
    documentationSummary: entry.documentationSummary,
    types: asTypeDocs(getChildren(db, id, SuggestionKind.Type)),
    methods: asFunctionDocs(getChildren(db, id, SuggestionKind.Method)),
  }),
}
