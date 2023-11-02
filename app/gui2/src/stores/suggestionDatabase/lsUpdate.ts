import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import {
  documentationData,
  type DocumentationData,
} from '@/stores/suggestionDatabase/documentation'
import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionEntryArgument,
  type SuggestionEntryScope,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import { assert, assertNever } from '@/util/assert'
import type { Doc } from '@/util/docParser'
import type { Icon } from '@/util/iconName'
import { type Opt } from '@/util/opt'
import {
  qnJoin,
  qnLastSegment,
  tryIdentifier,
  tryQualifiedName,
  type Identifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { Err, Ok, withContext, type Result } from '@/util/result'
import * as lsTypes from 'shared/languageServerTypes/suggestions'

interface UnfinishedEntry {
  kind: SuggestionKind
  definedIn?: QualifiedName
  memberOf?: QualifiedName
  isPrivate?: boolean
  isUnstable?: boolean
  name?: Identifier
  aliases?: string[]
  selfType?: Typename
  arguments?: SuggestionEntryArgument[]
  returnType?: Typename
  reexportedIn?: QualifiedName
  documentation?: Doc.Section[]
  scope?: SuggestionEntryScope
  iconName?: Icon
  groupIndex?: number | undefined
}

function setLsName(
  entry: UnfinishedEntry,
  name: string,
): entry is UnfinishedEntry & { name: Identifier } {
  const ident = tryIdentifier(name)
  if (!ident.ok) return false
  entry.name = ident.value
  return true
}

function setLsModule(
  entry: UnfinishedEntry & { name: Identifier },
  module: string,
): entry is UnfinishedEntry & { name: Identifier; definedIn: QualifiedName } {
  const qn = tryQualifiedName(module)
  if (!qn.ok) return false
  entry.definedIn = qn.value
  switch (entry.kind) {
    case SuggestionKind.Module:
      entry.name = qnLastSegment(qn.value)
      entry.returnType = qn.value
      break
    case SuggestionKind.Type:
      entry.returnType = qnJoin(qn.value, entry.name)
      break
  }
  return true
}

function setAsOwner(entry: UnfinishedEntry, type: string) {
  const qn = tryQualifiedName(type)
  if (qn.ok) {
    entry.memberOf = qn.value
  } else {
    delete entry.memberOf
  }
}

function setLsSelfType(entry: UnfinishedEntry, selfType: Typename, isStaticParam?: boolean) {
  const isStatic = isStaticParam ?? entry.selfType == null
  if (!isStatic) entry.selfType = selfType
  setAsOwner(entry, selfType)
}

function setLsReturnType(
  entry: UnfinishedEntry,
  returnType: Typename,
): asserts entry is UnfinishedEntry & { returnType: Typename } {
  entry.returnType = returnType
  if (entry.kind == SuggestionKind.Constructor) {
    setAsOwner(entry, returnType)
  }
}

function setLsReexported(
  entry: UnfinishedEntry,
  reexported: string,
): entry is UnfinishedEntry & { reexprotedIn: QualifiedName } {
  const qn = tryQualifiedName(reexported)
  if (!qn.ok) return false
  entry.reexportedIn = qn.value
  return true
}

function setLsDocumentation(
  entry: UnfinishedEntry & { definedIn: QualifiedName },
  documentation: Opt<string>,
  groups: Group[],
): asserts entry is UnfinishedEntry & { definedIn: QualifiedName } & DocumentationData {
  const data = documentationData(documentation, entry.definedIn, groups)
  Object.assign(entry, data)
  // Removing optional fields. I don't know a better way to do this.
  if (data.groupIndex == null) delete entry.groupIndex
  if (data.iconName == null) delete entry.iconName
}

export function entryFromLs(
  lsEntry: lsTypes.SuggestionEntry,
  groups: Group[],
): Result<SuggestionEntry> {
  return withContext(
    () => `when creating entry`,
    () => {
      switch (lsEntry.type) {
        case 'function': {
          const entry = { kind: SuggestionKind.Function }
          if (!setLsName(entry, lsEntry.name)) return Err('Invalid name')
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          setLsReturnType(entry, lsEntry.returnType)
          setLsDocumentation(entry, lsEntry.documentation, groups)
          return Ok({
            scope: lsEntry.scope,
            arguments: lsEntry.arguments,
            ...entry,
          })
        }
        case 'module': {
          const entry = {
            kind: SuggestionKind.Module,
            name: 'MODULE' as Identifier,
            arguments: [],
            returnType: '',
          }
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          if (lsEntry.reexport != null && !setLsReexported(entry, lsEntry.reexport))
            return Err('Invalid reexported module name')
          setLsDocumentation(entry, lsEntry.documentation, groups)
          assert(entry.returnType !== '') // Should be overwriten
          return Ok(entry)
        }
        case 'type': {
          const entry = { kind: SuggestionKind.Type, returnType: '' }
          if (!setLsName(entry, lsEntry.name)) return Err('Invalid name')
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          if (lsEntry.reexport != null && !setLsReexported(entry, lsEntry.reexport))
            return Err('Invalid reexported module name')
          setLsDocumentation(entry, lsEntry.documentation, groups)
          assert(entry.returnType !== '') // Should be overwriten
          return Ok({
            arguments: lsEntry.params,
            ...entry,
          })
        }
        case 'constructor': {
          const entry = { kind: SuggestionKind.Constructor }
          if (!setLsName(entry, lsEntry.name)) return Err('Invalid name')
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          if (lsEntry.reexport != null && !setLsReexported(entry, lsEntry.reexport))
            return Err('Invalid reexported module name')
          setLsDocumentation(entry, lsEntry.documentation, groups)
          setLsReturnType(entry, lsEntry.returnType)
          return Ok({
            arguments: lsEntry.arguments,
            ...entry,
          })
        }
        case 'method': {
          const entry = { kind: SuggestionKind.Method }
          if (!setLsName(entry, lsEntry.name)) return Err('Invalid name')
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          if (lsEntry.reexport != null && !setLsReexported(entry, lsEntry.reexport))
            return Err('Invalid reexported module name')
          setLsDocumentation(entry, lsEntry.documentation, groups)
          setLsSelfType(entry, lsEntry.selfType, lsEntry.isStatic)
          setLsReturnType(entry, lsEntry.returnType)
          return Ok({
            arguments: lsEntry.arguments,
            ...entry,
          })
        }
        case 'local': {
          const entry = { kind: SuggestionKind.Local, arguments: [] }
          if (!setLsName(entry, lsEntry.name)) return Err('Invalid name')
          if (!setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
          setLsReturnType(entry, lsEntry.returnType)
          setLsDocumentation(entry, lsEntry.documentation, groups)
          return Ok({
            scope: lsEntry.scope,
            ...entry,
          })
        }
        default:
          assertNever(lsEntry)
      }
    },
  )
}

function applyFieldUpdate<K extends string, T, R>(
  name: K,
  update: { [P in K]?: lsTypes.FieldUpdate<T> },
  updater: (newValue: T) => R,
): Result<Opt<R>> {
  const field = update[name]
  if (field == null) return Ok(null)
  return withContext(
    () => `when handling field "${name}" update`,
    () => {
      switch (field.tag) {
        case 'Set':
          if (field.value != null) {
            return Ok(updater(field.value))
          } else {
            return Err('Received "Set" update with no value')
          }
        case 'Remove':
          return Err(`Received "Remove" for non-optional field`)
        default:
          return Err(`Received field update with unknown value`)
      }
    },
  )
}

function applyPropertyUpdate<K extends string, T>(
  name: K,
  obj: { [P in K]: T },
  update: { [P in K]?: lsTypes.FieldUpdate<T> },
): Result<undefined> {
  const apply = applyFieldUpdate(name, update, (newValue) => {
    obj[name] = newValue
  })
  if (!apply.ok) return apply
  return Ok(undefined)
}

function applyOptPropertyUpdate<K extends string, T>(
  name: K,
  obj: { [P in K]?: T },
  update: { [P in K]?: lsTypes.FieldUpdate<T> },
) {
  const field = update[name]
  switch (field?.tag) {
    case 'Set':
      obj[name] = field.value
      break
    case 'Remove':
      delete obj[name]
      break
  }
}

function applyArgumentsUpdate(
  args: SuggestionEntryArgument[],
  update: lsTypes.SuggestionArgumentUpdate,
): Result<undefined> {
  switch (update.type) {
    case 'Add': {
      args.splice(update.index, 0, update.argument)
      return Ok(undefined)
    }
    case 'Remove': {
      args.splice(update.index, 1)
      return Ok(undefined)
    }
    case 'Modify': {
      return withContext(
        () => `when modifying argument with index ${update.index}`,
        () => {
          const arg = args[update.index]
          if (arg == null) return Err(`Wrong argument index ${update.index}`)
          const nameUpdate = applyPropertyUpdate('name', arg, update)
          if (!nameUpdate.ok) return nameUpdate
          const typeUpdate = applyFieldUpdate('reprType', update, (type) => {
            arg.type = type
          })
          if (!typeUpdate.ok) return typeUpdate
          const isSuspendedUpdate = applyPropertyUpdate('isSuspended', arg, update)
          if (!isSuspendedUpdate.ok) return isSuspendedUpdate
          const hasDefaultUpdate = applyPropertyUpdate('hasDefault', arg, update)
          if (!hasDefaultUpdate.ok) return hasDefaultUpdate
          applyOptPropertyUpdate('defaultValue', arg, update)
          return Ok(undefined)
        },
      )
    }
  }
}

export function applyUpdate(
  entries: SuggestionDb,
  update: lsTypes.SuggestionsDatabaseUpdate,
  groups: Group[],
): Result<undefined> {
  switch (update.type) {
    case 'Add': {
      return withContext(
        () => `when adding new entry ${JSON.stringify(update)}`,
        () => {
          const newEntry = entryFromLs(update.suggestion, groups)
          if (!newEntry.ok) return newEntry
          entries.set(update.id, newEntry.value)
          return Ok(undefined)
        },
      )
    }
    case 'Remove': {
      if (!entries.delete(update.id)) {
        return Err(`Received "Remove" suggestion database update for non-existing id ${update.id}.`)
      }
      return Ok(undefined)
    }
    case 'Modify': {
      return withContext(
        () => `when modifying entry to ${JSON.stringify(update)}`,
        () => {
          const entry = entries.get(update.id)
          if (entry == null) {
            return Err(`Entry with id ${update.id} does not exist.`)
          }

          for (const argumentUpdate of update.arguments ?? []) {
            const updateResult = applyArgumentsUpdate(entry.arguments, argumentUpdate)
            if (!updateResult.ok) return updateResult
          }

          const moduleUpdate = applyFieldUpdate('module', update, (module) =>
            setLsModule(entry, module),
          )
          if (!moduleUpdate.ok) return moduleUpdate
          if (moduleUpdate.value === false) return Err('Invalid module name')

          const selfTypeUpdate = applyFieldUpdate('selfType', update, (selfType) =>
            setLsSelfType(entry, selfType),
          )
          if (!selfTypeUpdate.ok) return selfTypeUpdate

          const returnTypeUpdate = applyFieldUpdate('returnType', update, (returnType) => {
            setLsReturnType(entry, returnType)
          })
          if (!returnTypeUpdate.ok) return returnTypeUpdate

          if (update.documentation != null)
            setLsDocumentation(entry, update.documentation.value, groups)

          applyOptPropertyUpdate('scope', entry, update)

          if (update.reexport != null) {
            if (update.reexport.value != null) {
              const reexport = tryQualifiedName(update.reexport.value)
              if (!reexport.ok) return reexport
              entry.reexportedIn = reexport.value
            } else {
              delete entry.reexportedIn
            }
          }

          return Ok(undefined)
        },
      )
    }
  }
}

export function applyUpdates(
  entries: SuggestionDb,
  updates: lsTypes.SuggestionsDatabaseUpdate[],
  groups: Group[],
) {
  for (const update of updates) {
    const updateResult = applyUpdate(entries, update, groups)
    if (!updateResult.ok) {
      updateResult.error.log()
      console.error(`Removing entry ${update.id}, because its state is unclear`)
      entries.delete(update.id)
    }
  }
}
