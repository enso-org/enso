import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import {
  SuggestionKind,
  type Doc,
  type SuggestionEntry,
  type SuggestionEntryArgument,
  type Typename,
  type SuggestionEntryScope,
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
  type Identifier,
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
  iconName?: string
  groupIndex?: number
}



class Update<Entry extends UnfinishedEntry>{
  entry: Entry
  groups: Group[]

  constructor(entry: Entry, groups: Group[]) {
    ;(this.entry = entry), (this.groups = groups)
  }

  
  setLsModule(module: string): Result<Update<Entry & {} > {
    const qn = tryQualifiedName(module)
    if (!qn.ok) return qn
    this.entry.definedIn = qn.value
    switch (this.entry.kind) {
      case SuggestionKind.Module:
        this.entry.name = qnLastSegment(qn.value)
        break
      case SuggestionKind.Type:
        this.entry.returnType = qnJoin(qn.value, this.entry.name)
        break
    }
    return Ok(undefined)
  }

  private setAsOwner(type: string) {
    const qn = tryQualifiedName(type)
    if (qn.ok) {
      this.entry.memberOf = qn.value
    } else {
      delete this.entry.memberOf
    }
  }

  setLsSelfType(selfType: string) {
    const isStatic = this.entry.selfType == null
    if (!isStatic) this.entry.selfType = selfType
    this.setAsOwner(selfType)
  }

  setLsReturnType(returnType: string) {
    this.entry.returnType = returnType
    if (this.entry.kind == SuggestionKind.Constructor) {
      this.setAsOwner(returnType)
    }
  }

  setLsDocumentation(documentation: string) {
    const data = documentationData(documentation, this.entry.definedIn, this.groups)
    Object.assign(this.entry, data)
  }
}

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

function documentationData(
  docs: Opt<string>,
  definedIn: QualifiedName,
  groups: Group[],
): {
  documentation: Doc.Section[]
  aliases: string[]
  iconName?: string
  groupIndex?: number
  isPrivate: boolean
  isUnstable: boolean
} {
  const documentation = docs != null ? parseDocs(docs) : []
  const groupName = tagValue(documentation, 'Group')
  const iconName = tagValue(documentation, 'Icon')
  const groupIndex = groupName ? getGroupIndex(groupName, definedIn, groups) : null

  return {
    documentation,
    aliases: tagValue(documentation, 'Alias')?.split(',') ?? [],
    ...(iconName != null ? { iconName } : {}),
    ...(groupIndex != null ? { groupIndex } : {}),
    isPrivate: isSome(tagValue(documentation, 'Private')),
    isUnstable:
      isSome(tagValue(documentation, 'Unstable')) || isSome(tagValue(documentation, 'Advanced')),
  }
}

function entryFromLs(lsEntry: lsTypes.SuggestionEntry, groups: Group[]): Result<SuggestionEntry> {
  return withContext(
    () => `when creating entry from ${lsEntry}`,
    () => {
      const definedIn = tryQualifiedName(lsEntry.module)
      if (!definedIn.ok) return definedIn

      const name =
        lsEntry.type == 'module' ? Ok(qnLastSegment(definedIn.value)) : tryIdentifier(lsEntry.name)
      if (!name.ok) return name

      const returnType = (() => {
        switch (lsEntry.type) {
          case 'type':
            return qnJoin(definedIn.value, name.value)
          case 'module':
            return definedIn.value
          default:
            return lsEntry.returnType
        }
      })()

      let entry: SuggestionEntry = {
        definedIn: definedIn.value,
        name: name.value,
        ...documentationData(lsEntry.documentation, definedIn.value, groups),
      }

      const memberOf = (() => {
        // Both self type and return type may be not a valid qualified name, because they may be a type
        // union or a type with parameters. In that case we cannot clearly point the owning type.
        switch (lsEntry.type) {
          case 'method': {
            const selfAsQn = tryQualifiedName(lsEntry.selfType)
            if (selfAsQn.ok) return selfAsQn.value
            else return null
          }
          case 'constructor': {
            const returnTypeAsQn = tryQualifiedName(lsEntry.returnType)
            if (returnTypeAsQn.ok) return returnTypeAsQn.value
            else return null
          }
          default:
            return null
        }
      })()

      const selfType = lsEntry.type == 'method' && !lsEntry.isStatic ? lsEntry.selfType : null

      const args = argumentsFromLs(lsEntry)

      const reexportedIn =
        lsEntry.type != 'function' && lsEntry.type != 'local' && lsEntry.reexport != null
          ? tryQualifiedName(lsEntry.reexport)
          : null
      if (reexportedIn != null && !reexportedIn.ok) return reexportedIn

      const scope =
        lsEntry.type == 'function' || lsEntry.type == 'local' ? lsEntry.scope : undefined

      return Ok({
        kind: kindFromLs(lsEntry),
        definedIn: definedIn.value,
        memberOf: memberOf ?? undefined,
        name: name.value,
        selfType,
        arguments: args,
        returnType,
        reexportedIn: reexportedIn?.value,
        scope,
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
        case 'Remove':
          return Err(`Cannot remove non-optional field`)
        default:
          return Ok(undefined)
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

          // Update Arguments
          for (const argumentUpdate of update.arguments ?? []) {
            const updateResult = applyArgumentsUpdate(entry.arguments, argumentUpdate)
            if (!updateResult.ok) return updateResult
          }

          

          // Others
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

  test.each([
    ['ALIAS Bar', 'Bar'],
    ['Some one section\n   But not tags here', null],
    ['GROUP different tag', null],
    ['PRIVATE\nGROUP Input\nALIAS Foo\n\nSeveral tags', 'Foo'],
  ])('Getting tag from docs case %#.', (doc, expected) => {
    const sections = parseDocs(doc)
    console.log(JSON.stringify(sections))
    expect(tagValue(sections, 'Alias')).toBe(expected)
  })
}
