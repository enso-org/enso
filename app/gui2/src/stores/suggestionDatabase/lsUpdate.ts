import { SuggestionDb } from '@/stores/suggestionDatabase'
import { SuggestionKind, type SuggestionEntry, type SuggestionEntryArgument, type SuggestionId } from '@/stores/suggestionDatabase/entry'
import type { Opt } from '@/util/opt'
import { tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import * as lsTypes from 'shared/languageServerTypes/suggestions'

function kindFromLs(lsEntry: lsTypes.SuggestionEntry) {
  switch (lsEntry.type) {
    case 'function': return SuggestionKind.Function
    case 'module': return SuggestionKind.Module
    case 'type': return SuggestionKind.Type
    case 'constructor': return SuggestionKind.Constructor
    case 'method': return SuggestionKind.Method
    case 'local': return SuggestionKind.Local
  }
}

// function tagValue(documentation: ): string {
//   return data.documentation.find((section: any) => section['Tag']?.tag === tag)?.Tag.body
// }



function memberOfAndSelfTypeFromLs(lsEntry: lsTypes.SuggestionEntry): Opt

function entryFromLs(id: SuggestionId, lsEntry: lsTypes.SuggestionEntry): Opt<SuggestionEntry> {
  function error(msg: string): null {
    console.error(`Invalid suggestion entry ${id} received from Language Server: ${msg}. Skipping.`)
    return null
  }

  const definedIn = tryQualifiedName(lsEntry.module)
  if (definedIn == null) return error("'module' is not a valid qualified name")

  let memberOf: QualifiedName | undefined;
  let selfType: QualifiedName | undefined;
  if (lsEntry.type == 'method') {
    memberOf = tryQualifiedName(lsEntry.selfType)
    if (memberOf == null) return error("'selfType' is not a valid qualified name")
    if (!lsEntry.isStatic) {
      selfType = memberOf
    }
  }

  return {
    kind: kindFromLs(lsEntry),
  definedIn,
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
}

function applyOptFieldUpdate<K extends string, T>(
  name: K,
  obj: { [P in K]?: T },
  update: Opt<lsTypes.FieldUpdate<T>>,
) {
  switch (update?.tag) {
    case 'Set':
      obj[name] = update.value
      break
    case 'Remove':
      obj[name] = undefined
      break
  }
}

function applyFieldUpdate<K extends string, T>(
  name: K,
  obj: { [P in K]: T },
  update: Opt<lsTypes.FieldUpdate<T>>,
) {
  switch (update?.tag) {
    case 'Set':
      if (update.value != null) {
        obj[name] = update.value
      } else {
        console.error(`Missing value on "Set" field update for ${name}`)
      }
      break
    case 'Remove':
      console.error(`Cannot remove field ${name}`)
      break
  }
}

function applyArgumentsUpdate(
  args: SuggestionEntryArgument[],
  update: lsTypes.SuggestionArgumentUpdate,
) {
  switch (update.type) {
    case 'Add': {
      args.splice(update.index, 0, update.argument)
      break
    }
    case 'Remove': {
      args.splice(update.index, 1)
      break
    }
    case 'Modify': {
      const arg = args[update.index]
      applyFieldUpdate('name', arg, update.name)
      applyFieldUpdate('type', arg, update.reprType)
      applyFieldUpdate('isSuspended', arg, update.isSuspended)
      applyFieldUpdate('hasDefault', arg, update.hasDefault)
      applyOptFieldUpdate('defaultValue', arg, update.defaultValue)
    }
  }
}

export function applyUpdate(entries: SuggestionDb, update: lsTypes.SuggestionsDatabaseUpdate) {
  switch (update.type) {
    case 'Add': {
      entries.set(update.id)
    }
    case 'Remove':
    case 'Modify':
  }
}
