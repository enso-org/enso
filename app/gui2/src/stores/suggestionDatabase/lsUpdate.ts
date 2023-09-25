import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  type Doc,
  type SuggestionEntry,
  type SuggestionEntryArgument,
} from '@/stores/suggestionDatabase/entry'
import { findIndexOpt } from '@/util/array'
import { parseDocs } from '@/util/ffi'
import { isSome, type Opt } from '@/util/opt'
import {
  qnJoin,
  qnLastSegment,
  tryIdentifier,
  tryQualifiedName,
  type QualifiedName,
} from '@/util/qualifiedName'
import { Err, Ok, withContext, type Result } from '@/util/result'
import * as lsTypes from 'shared/languageServerTypes/suggestions'

function kindFromLs(lsEntry: lsTypes.SuggestionEntry) {
  switch (lsEntry.type) {
    case 'function':
      return SuggestionKind.Function
    case 'module':
      return SuggestionKind.Module
    case 'type':
      return SuggestionKind.Type
    case 'constructor':
      return SuggestionKind.Constructor
    case 'method':
      return SuggestionKind.Method
    case 'local':
      return SuggestionKind.Local
  }
}

function isTagNamed(tag: string) {
  return (section: Doc.Section): section is { Tag: Doc.Section.Tag } => {
    return 'Tag' in section ? section.Tag.tag == tag : false
  }
}

function tagValue(doc: Doc.Section[], tag: string): Opt<string> {
  const tagSection = doc.find(isTagNamed(tag))
  if (tagSection == null) return null
  return tagSection.Tag.body
}

function argumentsFromLs(lsEntry: lsTypes.SuggestionEntry): SuggestionEntryArgument[] {
  switch (lsEntry.type) {
    case 'constructor':
    case 'method':
    case 'function':
      return lsEntry.arguments
    case 'type':
      return lsEntry.params
    default:
      return []
  }
}

function getGroupIndex(
  groupName: string,
  entryModule: QualifiedName,
  groups: Group[],
): Opt<number> {
  let normalized: string
  if (groupName.indexOf('.') >= 0) {
    normalized = groupName
  } else {
    const project = /^[^.]+\.[^.]+/.exec(entryModule)
    if (project == null) return null
    normalized = `${project}.${groupName}`
  }
  return findIndexOpt(groups, (group) => `${group.project}.${group.name}` == normalized)
}

function entryFromLs(lsEntry: lsTypes.SuggestionEntry, groups: Group[]): Result<SuggestionEntry> {
  return withContext(
    () => `when creating entry from ${lsEntry}`,
    () => {
      const documentation = lsEntry.documentation != null ? parseDocs(lsEntry.documentation) : []
      const aliases = tagValue(documentation, 'Alias')?.split(',') ?? []
      const iconName = tagValue(documentation, 'Icon')
      const groupName = tagValue(documentation, 'Group')

      const definedIn = tryQualifiedName(lsEntry.module)
      if (!definedIn.ok) return definedIn

      const name =
        lsEntry.type == 'module' ? Ok(qnLastSegment(definedIn.value)) : tryIdentifier(lsEntry.name)
      if (!name.ok) return name

      const memberOf = lsEntry.type == 'method' ? tryQualifiedName(lsEntry.selfType) : undefined
      if (memberOf != null && !memberOf.ok) return memberOf

      const selfType =
        lsEntry.type == 'method' && !lsEntry.isStatic && memberOf ? memberOf.value : undefined

      const args = argumentsFromLs(lsEntry)

      const returnType = (() => {
        switch (lsEntry.type) {
          case 'type':
            return Ok(qnJoin(definedIn.value, name.value))
          case 'module':
            return Ok(definedIn.value)
          default:
            return tryQualifiedName(lsEntry.returnType)
        }
      })()
      if (!returnType.ok) return returnType

      const reexportedIn =
        lsEntry.type != 'function' && lsEntry.type != 'local' && lsEntry.reexport != null
          ? tryQualifiedName(lsEntry.reexport)
          : undefined
      if (reexportedIn != null && !reexportedIn.ok) return reexportedIn

      const scope =
        lsEntry.type == 'function' || lsEntry.type == 'local' ? lsEntry.scope : undefined

      const groupIndex = groupName ? getGroupIndex(groupName, definedIn.value, groups) : undefined

      return Ok({
        kind: kindFromLs(lsEntry),
        definedIn: definedIn.value,
        memberOf: memberOf?.value,
        isPrivate: isSome(tagValue(documentation, 'PRIVATE')),
        isUnstable: isSome(tagValue(documentation, 'PRIVATE')),
        name: name.value,
        aliases,
        selfType,
        arguments: args,
        returnType: returnType?.value,
        reexportedIn: reexportedIn?.value,
        documentation,
        scope,
        iconName: iconName ?? undefined,
        groupIndex: groupIndex ?? undefined,
      })
    },
  )
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
): Result<undefined> {
  return withContext(
    () => `when updating field "${name}"`,
    () => {
      switch (update?.tag) {
        case 'Set':
          if (update.value != null) {
            obj[name] = update.value
            return Ok(undefined)
          } else {
            return Err('Received "Set" update with no value')
          }
        default:
        case 'Remove':
          return Err(`Cannot remove non-optional field`)
      }
    },
  )
}

function mapFieldUpdate<T, U>(
  update: Opt<lsTypes.FieldUpdate<T>>,
  f: (value: T) => U,
): Opt<lsTypes.FieldUpdate<U>> {
  if (update == null) return null
  if (update.value == null) return { tag: update.tag }
  return { tag: update.tag, value: f(update.value) }
}

function tryMapFieldUpdate<T, U, E>(
  update: Opt<lsTypes.FieldUpdate<T>>,
  f: (value: T) => Result<U, E>,
): Result<Opt<lsTypes.FieldUpdate<U>>, E> {
  if (update == null) return Ok(null)
  if (update.value == null) return Ok({ tag: update.tag })
  const newValue = f(update.value)
  if (newValue.ok) return Ok({ tag: update.tag, value: newValue.value })
  else return newValue
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
          const nameUpdate = applyFieldUpdate('name', arg, update.name)
          if (!nameUpdate.ok) return nameUpdate
          const typeUpdate = applyFieldUpdate('type', arg, update.reprType)
          if (!typeUpdate.ok) return typeUpdate
          const isSuspendedUpdate = applyFieldUpdate('isSuspended', arg, update.isSuspended)
          if (!isSuspendedUpdate.ok) return isSuspendedUpdate
          const hasDefaultUpdate = applyFieldUpdate('hasDefault', arg, update.hasDefault)
          if (!hasDefaultUpdate.ok) return hasDefaultUpdate
          applyOptFieldUpdate('defaultValue', arg, update.defaultValue)
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
        () => `when adding new entry with id ${update.id}`,
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
        () => `when modifying entry with id ${update.id}`,
        () => {
          const entry = entries.get(update.id)
          if (entry == null) {
            return Err(`Entry with id ${update.id} does not exist.`)
          }
          for (const argumentUpdate of update.arguments ?? []) {
            const updateResult = applyArgumentsUpdate(entry.arguments, argumentUpdate)
            if (!updateResult.ok) return updateResult
          }
          const definedIn = tryMapFieldUpdate(update.module, tryQualifiedName)
          if (!definedIn.ok) return definedIn
          const definedInUpdate = applyFieldUpdate('definedIn', entry, definedIn.value)
          if (!definedInUpdate.ok) return definedInUpdate
          const selfType = tryMapFieldUpdate(update.selfType, tryQualifiedName)
          if (!selfType.ok) return selfType
          applyOptFieldUpdate('memberOf', entry, selfType.value)
          applyOptFieldUpdate('selfType', entry, selfType.value)
          const returnType = tryMapFieldUpdate(update.returnType, tryQualifiedName)
          if (!returnType.ok) return returnType
          applyOptFieldUpdate('returnType', entry, selfType.value)
          const documentation = mapFieldUpdate(update.documentation, parseDocs)
          applyOptFieldUpdate('documentation', entry, documentation)
          applyOptFieldUpdate('scope', entry, update.scope)
          const reexport = tryMapFieldUpdate(update.reexport, tryQualifiedName)
          if (!reexport.ok) return reexport
          applyOptFieldUpdate('reexportedIn', entry, reexport.value)
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

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test.each([{ doc: '## Foo Bar\n', expected: 'Bar' }])(
    'Getting tag from docs case %#.',
    ({ doc, expected }) => {
      const sections = parseDocs(doc)
      expect(tagValue(sections, 'Foo')).toBe(expected)
    },
  )
}
