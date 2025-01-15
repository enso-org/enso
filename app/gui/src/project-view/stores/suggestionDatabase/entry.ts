import type { Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconMetadata/iconName'
import type { IdentifierOrOperatorIdentifier, QualifiedName } from '@/util/qualifiedName'
import { qnJoin, qnParent, qnSegments } from '@/util/qualifiedName'
import type { MethodPointer } from 'ydoc-shared/languageServerTypes'
import type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
} from 'ydoc-shared/languageServerTypes/suggestions'

export type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
  SuggestionId,
} from 'ydoc-shared/languageServerTypes/suggestions'

/**
 * An alias type for typename (for entry fields like `returnType`).
 *
 * It's not QualifiedName, because it may be a type with parameters, or
 * a type union.
 */
export type Typename = string

// The kind of a suggestion.
export enum SuggestionKind {
  Module = 'Module',
  Type = 'Type',
  Constructor = 'Constructor',
  Method = 'Method',
  Function = 'Function',
  Local = 'Local',
}

export interface SuggestionEntry {
  kind: SuggestionKind
  /** A module where the suggested object is defined. */
  definedIn: QualifiedName
  /** A type or module this method or constructor belongs to. */
  memberOf?: QualifiedName
  isPrivate: boolean
  isUnstable: boolean
  name: IdentifierOrOperatorIdentifier
  aliases: string[]
  /** A type of the "self" argument. This field is present only for instance methods. */
  selfType?: Typename
  /**
   * Argument lists of suggested object (atom or function). If the object does not take any
   * arguments, the list is empty.
   */
  arguments: SuggestionEntryArgument[]
  /** A type returned by the suggested object. */
  returnType: Typename
  /** Qualified name of the parent type. */
  parentType?: QualifiedName
  /** A least-nested module reexporting this entity. */
  reexportedIn?: QualifiedName
  documentation: Doc.Section[]
  /** A scope where this suggestion is visible. */
  scope?: SuggestionEntryScope
  /** A name of a custom icon to use when displaying the entry. */
  iconName?: Icon
  /** An index of a group from group list in suggestionDb store this entry belongs to. */
  groupIndex?: number
  /** A list of annotations. They are present for methods and constructors only. */
  annotations: string[]
}

/** Get the fully qualified name of the `SuggestionEntry`, disregarding reexports. */
export function entryQn(entry: SuggestionEntry): QualifiedName {
  if (entry.kind == SuggestionKind.Module) {
    return entry.definedIn
  } else {
    const owner = entryOwnerQn(entry)
    return owner ? qnJoin(owner, entry.name) : entry.name
  }
}

/** Get the MethodPointer pointing to definition represented by the entry. */
export function entryMethodPointer(entry: SuggestionEntry): MethodPointer | undefined {
  if (entry.kind !== SuggestionKind.Method || !entry.memberOf) return
  return {
    module: entry.definedIn,
    definedOnType: entry.memberOf,
    name: entry.name,
  }
}

/** TODO: Add docs */
export function entryOwnerQn(entry: SuggestionEntry): QualifiedName | null {
  if (entry.kind == SuggestionKind.Module) {
    return qnParent(entry.definedIn)
  } else {
    return entry.memberOf ?? entry.definedIn
  }
}

const DOCUMENTATION_ROOT = 'https://help.enso.org/docs/api'

/** TODO: Add docs */
export function suggestionDocumentationUrl(entry: SuggestionEntry): string | undefined {
  if (entry.kind !== SuggestionKind.Method && entry.kind !== SuggestionKind.Function) return
  const location = entry.memberOf ?? entry.definedIn
  const segments: string[] = qnSegments(location)
  if (segments[0] !== 'Standard') return
  if (segments.length < 3) return
  const namespace = segments[0]
  segments[0] = DOCUMENTATION_ROOT
  segments[1] = `${namespace}.${segments[1]}`
  segments[segments.length - 1] += `.${entry.name}`
  return segments.join('/')
}

/** `true` if calling the function without providing a value for this argument will result in an error. */
export function isRequiredArgument(info: SuggestionEntryArgument) {
  return !!info.defaultValue?.startsWith('Missing_Argument.')
}
