import { bail } from '@/util/assert'
import {
  qnLastSegment,
  qnParent,
  type Identifier,
  type QualifiedName,
  assumeQualifiedName,
  assumeIdentifier,
  qnSplit,
} from '@/util/qualifiedName'

export type SuggestionId = number

export type UUID = string

// The kind of a suggestion.
export enum SuggestionKind {
  Module = 'Module',
  Type = 'Type',
  Constructor = 'Constructor',
  Method = 'Method',
  Function = 'Function',
  Local = 'Local',
}

// The argument of a constructor, method or function suggestion.
export interface SuggestionEntryArgument {
  /** The argument name. */
  name: string
  /** The argument type. String 'Any' is used to specify generic types. */
  type: string
  /** Indicates whether the argument is lazy. */
  isSuspended: boolean
  /** Indicates whether the argument has default value. */
  hasDefault: boolean
  /** Optional default value. */
  defaultValue?: string
  /** Optional list of possible values that this argument takes. */
  tagValues?: string[]
}

export interface Position {
  /**
   * Line position in a document (zero-based).
   */
  line: number

  /**
   * Character offset on a line in a document (zero-based). Assuming that the
   * line is represented as a string, the `character` value represents the gap
   * between the `character` and `character + 1`.
   *
   * If the character value is greater than the line length it defaults back to
   * the line length.
   */
  character: number
}

// The definition scope
export interface SuggestionEntryScope {
  // The start position of the definition scope
  start: Position
  // The end position of the definition scope
  end: Position
}

export interface SuggestionEntry {
  kind: SuggestionKind
  /// A module where the suggested object is defined.
  definedIn: QualifiedName
  /// A type or module this method or constructor belongs to.
  memberOf?: QualifiedName
  isPrivate: boolean
  isUnstable: boolean
  /// A name of suggested object.
  name: Identifier
  /// A list of aliases.
  aliases: string[]
  /// A type of the "self" argument. This field is present only for instance methods.
  selfType?: QualifiedName
  /// Argument lists of suggested object (atom or function). If the object does not take any
  /// arguments, the list is empty.
  arguments: SuggestionEntryArgument[]
  /// A type returned by the suggested object.
  returnType: QualifiedName
  /// A module reexporting this entity.
  reexportedIn?: QualifiedName
  /// A list of documentation sections associated with object.
  documentation: string
  /// A scope where this suggestion is visible.
  scope?: SuggestionEntryScope
  /// A name of a custom icon to use when displaying the entry.
  iconName?: string
  /// A name of a group this entry belongs to.
  groupIndex?: number
}

function makeSimpleEntry(
  kind: SuggestionKind,
  definedIn: QualifiedName,
  name: Identifier,
  returnType: QualifiedName,
): SuggestionEntry {
  return {
    kind,
    definedIn: assumeQualifiedName(definedIn),
    name: assumeIdentifier(name),
    isPrivate: false,
    isUnstable: false,
    aliases: [],
    arguments: [],
    returnType: assumeQualifiedName(returnType),
    documentation: '',
  }
}

export function makeModule(fullyQualifiedName: string): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  return makeSimpleEntry(SuggestionKind.Module, fqn, qnLastSegment(fqn), fqn)
}

export function makeType(fullyQualifiedName: string): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  const [definedIn, name] = qnSplit(fqn)
  return makeSimpleEntry(SuggestionKind.Type, definedIn ?? bail('Invalid type name'), name, fqn)
}

export function makeCon(fullyQualifiedName: string): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  const [maybeType, name] = qnSplit(fqn)
  const type = maybeType ?? bail('Invalid constructor name')
  const definedIn = qnParent(type) ?? bail('Invalid constructor name')
  return {
    memberOf: type,
    ...makeSimpleEntry(SuggestionKind.Constructor, definedIn, name, type),
  }
}

export function makeMethod(
  fullyQualifiedName: string,
  returnType: string = 'Any',
): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  const [maybeType, name] = qnSplit(fqn)
  const type = maybeType ?? bail('Invalid method name')
  const definedIn = qnParent(type) ?? bail('Invalid method name')
  return {
    memberOf: type,
    selfType: type,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, assumeQualifiedName(returnType)),
  }
}

export function makeStaticMethod(
  fullyQualifiedName: string,
  returnType: string = 'Any',
): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  const [maybeType, name] = qnSplit(fqn)
  const type = maybeType ?? bail('Invalid method name')
  const definedIn = qnParent(type) ?? bail('Invalid method name')
  return {
    memberOf: type,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, assumeQualifiedName(returnType)),
  }
}

export function makeModuleMethod(
  fullyQualifiedName: string,
  returnType: string = 'Any',
): SuggestionEntry {
  const fqn = assumeQualifiedName(fullyQualifiedName)
  const [maybeDefinedIn, name] = qnSplit(fqn)
  const definedIn = maybeDefinedIn ?? bail('Invalid method name')
  return {
    memberOf: definedIn,
    ...makeSimpleEntry(SuggestionKind.Method, definedIn, name, assumeQualifiedName(returnType)),
  }
}

export function makeFunction(
  definedIn: string,
  name: string,
  returnType: string = 'Any',
): SuggestionEntry {
  return makeSimpleEntry(
    SuggestionKind.Function,
    assumeQualifiedName(definedIn),
    assumeIdentifier(name),
    assumeQualifiedName(returnType),
  )
}

export function makeLocal(
  definedIn: string,
  name: string,
  returnType: string = 'Any',
): SuggestionEntry {
  return makeSimpleEntry(
    SuggestionKind.Local,
    assumeQualifiedName(definedIn),
    assumeIdentifier(name),
    assumeQualifiedName(returnType),
  )
}
