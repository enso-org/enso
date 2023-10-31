import { assert } from '@/util/assert'
import type { Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconName'
import {
  isIdentifier,
  isQualifiedName,
  qnJoin,
  qnLastSegment,
  qnParent,
  qnSplit,
  tryQualifiedName,
  type Identifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { unwrap } from '@/util/result'
import type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
} from 'shared/languageServerTypes/suggestions'
export type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
  SuggestionId,
} from 'shared/languageServerTypes/suggestions'

/** An alias type for typename (for entry fields like `returnType`).
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
  name: Identifier
  aliases: string[]
  /** A type of the "self" argument. This field is present only for instance methods. */
  selfType?: Typename
  /** Argument lists of suggested object (atom or function). If the object does not take any
   * arguments, the list is empty. */
  arguments: SuggestionEntryArgument[]
  /** A type returned by the suggested object. */
  returnType: Typename
  /** A least-nested module reexporting this entity. */
  reexportedIn?: QualifiedName
  documentation: Doc.Section[]
  /** A scope where this suggestion is visible. */
  scope?: SuggestionEntryScope
  /** A name of a custom icon to use when displaying the entry. */
  iconName?: Icon
  /** An index of a group from group list in suggestionDb store this entry belongs to. */
  groupIndex?: number
}

/**
 * Get the fully qualified name of the `SuggestionEntry`, disregarding reexports.
 */
export function entryQn(entry: SuggestionEntry): QualifiedName {
  if (entry.kind == SuggestionKind.Module) {
    return entry.definedIn
  } else if (entry.memberOf) {
    return qnJoin(entry.memberOf, entry.name)
  } else {
    return qnJoin(entry.definedIn, unwrap(tryQualifiedName(entry.name)))
  }
}

function makeSimpleEntry(
  kind: SuggestionKind,
  definedIn: QualifiedName,
  name: Identifier,
  returnType: QualifiedName,
): SuggestionEntry {
  return {
    kind,
    definedIn,
    name,
    isPrivate: false,
    isUnstable: false,
    aliases: [],
    arguments: [],
    returnType,
    documentation: [],
  }
}

export function makeModule(fqn: string): SuggestionEntry {
  assert(isQualifiedName(fqn))
  return makeSimpleEntry(SuggestionKind.Module, fqn, qnLastSegment(fqn), fqn)
}

export function makeType(fqn: string): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [definedIn, name] = qnSplit(fqn)
  assert(definedIn != null)
  return makeSimpleEntry(SuggestionKind.Type, definedIn, name, fqn)
}

export function makeCon(fqn: string): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return {
    memberOf: type,
    ...makeSimpleEntry(SuggestionKind.Constructor, definedIn, name, type),
  }
}

export function makeMethod(fqn: string, returnType: string = 'Any'): SuggestionEntry {
  assert(isQualifiedName(fqn))
  assert(isQualifiedName(returnType))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return {
    memberOf: type,
    selfType: type,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, returnType),
  }
}

export function makeStaticMethod(fqn: string, returnType: string = 'Any'): SuggestionEntry {
  assert(isQualifiedName(fqn))
  assert(isQualifiedName(returnType))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return {
    memberOf: type,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, returnType),
  }
}

export function makeModuleMethod(fqn: string, returnType: string = 'Any'): SuggestionEntry {
  assert(isQualifiedName(fqn))
  assert(isQualifiedName(returnType))
  const [definedIn, name] = qnSplit(fqn)
  assert(definedIn != null)
  return {
    memberOf: definedIn,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, returnType),
  }
}

export function makeFunction(
  definedIn: string,
  name: string,
  returnType: string = 'Any',
): SuggestionEntry {
  assert(isQualifiedName(definedIn))
  assert(isIdentifier(name))
  assert(isQualifiedName(returnType))
  return makeSimpleEntry(SuggestionKind.Function, definedIn, name, returnType)
}

export function makeLocal(
  definedIn: string,
  name: string,
  returnType: string = 'Any',
): SuggestionEntry {
  assert(isQualifiedName(definedIn))
  assert(isIdentifier(name))
  assert(isQualifiedName(returnType))
  return makeSimpleEntry(SuggestionKind.Local, definedIn, name, returnType)
}
