import { type DocumentationData } from '@/stores/suggestionDatabase/documentation'
import {
  qnSegments,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import type { MethodPointer } from 'ydoc-shared/languageServerTypes'
import {
  type SuggestionEntryArgument,
  type SuggestionEntryScope,
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

export interface SuggestionEntryCommon extends DocumentationData {
  readonly kind: SuggestionKind
  /** A module where the suggested object is defined. */
  definedIn: QualifiedName
  name: IdentifierOrOperatorIdentifier
  /** A type returned by the suggested object. */
  returnType: Typename
  /** The fully qualified name of the `SuggestionEntry`, disregarding reexports. */
  definitionPath: QualifiedName
}

interface Reexportable {
  /** A least-nested module reexporting this entity. */
  reexportedIn: QualifiedName | undefined
}

interface Scoped {
  /** A scope where this suggestion is visible. */
  scope: SuggestionEntryScope | undefined
}

interface Annotatable {
  /** A list of annotations. They are present for methods and constructors only. */
  annotations: string[]
}

interface TakesArguments {
  /** Argument lists of suggested object (atom or function). */
  arguments: SuggestionEntryArgument[]
}

interface IsMemberOf {
  /**
   * A type or module this method or constructor belongs to. This will not be `undefined` unless the value was an
   * invalid qualified name.
   */
  memberOf: QualifiedName | undefined
}

export interface ModuleSuggestionEntry extends SuggestionEntryCommon, Reexportable {
  readonly kind: SuggestionKind.Module
}

export interface TypeSuggestionEntry extends SuggestionEntryCommon, Reexportable, TakesArguments {
  readonly kind: SuggestionKind.Type
  /** Qualified name of the parent type. */
  parentType: QualifiedName | undefined
}

export interface ConstructorSuggestionEntry
  extends SuggestionEntryCommon,
    Reexportable,
    Annotatable,
    TakesArguments,
    IsMemberOf {
  readonly kind: SuggestionKind.Constructor
}

export interface MethodSuggestionEntry
  extends SuggestionEntryCommon,
    Reexportable,
    Annotatable,
    TakesArguments,
    IsMemberOf {
  readonly kind: SuggestionKind.Method
  /** Type of the "self" argument. */
  selfType: Typename | undefined
}

export interface FunctionSuggestionEntry extends SuggestionEntryCommon, Scoped, TakesArguments {
  readonly kind: SuggestionKind.Function
}

export interface LocalSuggestionEntry extends SuggestionEntryCommon, Scoped {
  readonly kind: SuggestionKind.Local
}

export type SuggestionEntry =
  | ModuleSuggestionEntry
  | TypeSuggestionEntry
  | ConstructorSuggestionEntry
  | MethodSuggestionEntry
  | FunctionSuggestionEntry
  | LocalSuggestionEntry

/**
 * A type that can be called. This includes every suggestion kind that takes arguments, except
 * {@link SuggestionKind.Type}.
 */
export type CallableSuggestionEntry =
  | MethodSuggestionEntry
  | ConstructorSuggestionEntry
  | FunctionSuggestionEntry

/** Type predicate for {@link CallableSuggestionEntry}. */
export function entryIsCallable(entry: SuggestionEntry): entry is CallableSuggestionEntry {
  return (
    entry.kind === SuggestionKind.Method ||
    entry.kind === SuggestionKind.Function ||
    entry.kind === SuggestionKind.Constructor
  )
}

/** Predicate for types that can have annotated arguments. */
export function entryIsAnnotatable(
  entry: SuggestionEntry,
): entry is MethodSuggestionEntry | ConstructorSuggestionEntry {
  return entry.kind === SuggestionKind.Method || entry.kind === SuggestionKind.Constructor
}

/** Predicate for members that can be called on a type. */
export function entryIsStatic(
  entry: SuggestionEntry,
): entry is ConstructorSuggestionEntry | (MethodSuggestionEntry & { selfType: undefined }) {
  return (
    entry.kind === SuggestionKind.Constructor ||
    (entry.kind === SuggestionKind.Method && entry.selfType == null)
  )
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

const DOCUMENTATION_ROOT = 'https://help.enso.org/docs/api'

/** TODO: Add docs */
export function suggestionDocumentationUrl(entry: SuggestionEntry): string | undefined {
  if (entry.kind !== SuggestionKind.Method && entry.kind !== SuggestionKind.Function) return
  const location = entry.definitionPath
  const segments: string[] = qnSegments(location)
  if (segments[0] !== 'Standard') return
  if (segments.length < 3) return
  const project = `${segments[0]}.${segments[1]}`
  return [DOCUMENTATION_ROOT, project, ...segments.slice(2)].join('/')
}

/** `true` if calling the function without providing a value for this argument will result in an error. */
export function isRequiredArgument(info: SuggestionEntryArgument) {
  return !!info.defaultValue?.startsWith('Missing_Argument.')
}
