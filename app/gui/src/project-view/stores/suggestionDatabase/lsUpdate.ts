import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import {
  documentationData,
  type DocumentationData,
} from '@/stores/suggestionDatabase/documentation'
import {
  SuggestionKind,
  type SuggestionEntry,
  type SuggestionEntryArgument,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import { assert, assertNever } from '@/util/assert'
import { type Opt } from '@/util/data/opt'
import { Err, Ok, withContext, type Result } from '@/util/data/result'
import {
  normalizeQualifiedName,
  qnJoin,
  qnLastSegment,
  tryIdentifierOrOperatorIdentifier,
  tryQualifiedName,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { toValue, type DeepReadonly } from 'vue'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import {
  SuggestionArgumentUpdate,
  SuggestionsDatabaseUpdate,
} from 'ydoc-shared/languageServerTypes/suggestions'

interface UnfinishedEntry extends Partial<SuggestionEntry> {
  kind: SuggestionKind
}

/** Interprets language server messages to create and update suggestion database entries. */
export class SuggestionUpdateProcessor {
  /** Constructor. */
  constructor(private readonly groups: ToValue<DeepReadonly<Group[]>>) {}

  private setLsName(
    entry: UnfinishedEntry,
    name: string,
  ): entry is UnfinishedEntry & { name: IdentifierOrOperatorIdentifier } {
    const ident = tryIdentifierOrOperatorIdentifier(name)
    if (!ident.ok) return false
    entry.name = ident.value
    return true
  }

  private setLsModule(
    entry: UnfinishedEntry & { name: IdentifierOrOperatorIdentifier },
    module: string,
  ): entry is UnfinishedEntry & { name: IdentifierOrOperatorIdentifier; definedIn: QualifiedName } {
    const qn = tryQualifiedName(module)
    if (!qn.ok) return false
    const normalizedQn = normalizeQualifiedName(qn.value)
    entry.definedIn = normalizedQn
    switch (entry.kind) {
      case SuggestionKind.Module:
        entry.name = qnLastSegment(normalizedQn)
        entry.returnType = normalizedQn
        break
      case SuggestionKind.Type:
        entry.returnType = qnJoin(normalizedQn, entry.name)
        break
    }
    return true
  }

  private setAsOwner(entry: UnfinishedEntry, type: string) {
    const qn = tryQualifiedName(type)
    if (qn.ok) {
      entry.memberOf = normalizeQualifiedName(qn.value)
    } else {
      delete entry.memberOf
    }
  }

  private setLsSelfType(entry: UnfinishedEntry, selfType: Typename, isStaticParam?: boolean) {
    const isStatic = isStaticParam ?? entry.selfType == null
    if (!isStatic) entry.selfType = selfType
    this.setAsOwner(entry, selfType)
  }

  private setLsReturnType(
    entry: UnfinishedEntry,
    returnType: Typename,
  ): asserts entry is UnfinishedEntry & { returnType: Typename } {
    entry.returnType = returnType
    if (entry.kind == SuggestionKind.Constructor) {
      this.setAsOwner(entry, returnType)
    }
  }

  private setLsReexported(
    entry: UnfinishedEntry,
    reexported: string,
  ): entry is UnfinishedEntry & { reexprotedIn: QualifiedName } {
    const qn = tryQualifiedName(reexported)
    if (!qn.ok) return false
    entry.reexportedIn = normalizeQualifiedName(qn.value)
    return true
  }

  private setLsParentType(
    entry: UnfinishedEntry,
    parentType: string,
  ): entry is UnfinishedEntry & { parentType: QualifiedName } {
    const qn = tryQualifiedName(parentType)
    if (!qn.ok) return false
    entry.parentType = normalizeQualifiedName(qn.value)
    return true
  }

  private setLsDocumentation(
    entry: UnfinishedEntry & { definedIn: QualifiedName },
    documentation: Opt<string>,
  ): asserts entry is UnfinishedEntry & { definedIn: QualifiedName } & DocumentationData {
    const data = documentationData(documentation, entry.definedIn, toValue(this.groups))
    Object.assign(entry, data)
    // Removing optional fields. I don't know a better way to do this.
    if (data.groupIndex == null) delete entry.groupIndex
    if (data.iconName == null) delete entry.iconName
  }

  /** Create a suggestion DB entry from data provided by the given language server. */
  entryFromLs(lsEntry: lsTypes.SuggestionEntry): Result<SuggestionEntry> {
    return withContext(
      () => `when creating entry`,
      () => {
        switch (lsEntry.type) {
          case 'function': {
            const entry = {
              kind: SuggestionKind.Function,
              annotations: [],
            }
            if (!this.setLsName(entry, lsEntry.name)) return Err('Invalid name')
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            this.setLsReturnType(entry, lsEntry.returnType)
            this.setLsDocumentation(entry, lsEntry.documentation)
            return Ok({
              scope: lsEntry.scope,
              arguments: lsEntry.arguments,
              ...entry,
            })
          }
          case 'module': {
            const entry = {
              kind: SuggestionKind.Module,
              name: 'MODULE' as IdentifierOrOperatorIdentifier,
              arguments: [],
              returnType: '',
              annotations: [],
            }
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            if (lsEntry.reexport != null && !this.setLsReexported(entry, lsEntry.reexport))
              return Err('Invalid reexported module name')
            this.setLsDocumentation(entry, lsEntry.documentation)
            assert(entry.returnType !== '') // Should be overwriten
            return Ok(entry)
          }
          case 'type': {
            const entry = {
              kind: SuggestionKind.Type,
              returnType: '',
              annotations: [],
            }
            if (!this.setLsName(entry, lsEntry.name)) return Err('Invalid name')
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            if (lsEntry.reexport != null && !this.setLsReexported(entry, lsEntry.reexport))
              return Err('Invalid reexported module name')
            if (lsEntry.parentType != null && !this.setLsParentType(entry, lsEntry.parentType))
              return Err('Invalid parent type')
            this.setLsDocumentation(entry, lsEntry.documentation)
            assert(entry.returnType !== '') // Should be overwriten
            return Ok({
              arguments: lsEntry.params,
              ...entry,
            })
          }
          case 'constructor': {
            const entry = { kind: SuggestionKind.Constructor }
            if (!this.setLsName(entry, lsEntry.name)) return Err('Invalid name')
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            if (lsEntry.reexport != null && !this.setLsReexported(entry, lsEntry.reexport))
              return Err('Invalid reexported module name')
            this.setLsDocumentation(entry, lsEntry.documentation)
            this.setLsReturnType(entry, lsEntry.returnType)
            return Ok({
              arguments: lsEntry.arguments,
              annotations: lsEntry.annotations,
              ...entry,
            })
          }
          case 'method': {
            const entry = { kind: SuggestionKind.Method }
            if (!this.setLsName(entry, lsEntry.name)) return Err('Invalid name')
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            if (lsEntry.reexport != null && !this.setLsReexported(entry, lsEntry.reexport))
              return Err('Invalid reexported module name')
            this.setLsDocumentation(entry, lsEntry.documentation)
            this.setLsSelfType(entry, lsEntry.selfType, lsEntry.isStatic)
            this.setLsReturnType(entry, lsEntry.returnType)
            return Ok({
              arguments: lsEntry.arguments,
              annotations: lsEntry.annotations,
              ...entry,
            })
          }
          case 'local': {
            const entry = {
              kind: SuggestionKind.Local,
              arguments: [],
              annotations: [],
            }
            if (!this.setLsName(entry, lsEntry.name)) return Err('Invalid name')
            if (!this.setLsModule(entry, lsEntry.module)) return Err('Invalid module name')
            this.setLsReturnType(entry, lsEntry.returnType)
            this.setLsDocumentation(entry, lsEntry.documentation)
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

  private applyFieldUpdate<K extends string, T, R>(
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

  private applyPropertyUpdate<K extends string, T>(
    name: K,
    obj: { [P in K]: T },
    update: { [P in K]?: lsTypes.FieldUpdate<T> },
  ): Result<void> {
    const apply = this.applyFieldUpdate(name, update, (newValue) => {
      obj[name] = newValue
    })
    if (!apply.ok) return apply
    return Ok()
  }

  private applyOptPropertyUpdate<K extends string, T>(
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

  private applyArgumentsUpdate(
    args: SuggestionEntryArgument[],
    update: lsTypes.SuggestionArgumentUpdate,
  ): Result<void> {
    switch (update.type) {
      case 'Add': {
        args.splice(update.index, 0, update.argument)
        return Ok()
      }
      case 'Remove': {
        args.splice(update.index, 1)
        return Ok()
      }
      case 'Modify': {
        return withContext(
          () => `when modifying argument with index ${update.index}`,
          () => {
            const arg = args[update.index]
            if (arg == null) return Err(`Wrong argument index ${update.index}`)
            return this.modifyArgument(arg, update)
          },
        )
      }
    }
  }

  private modifyArgument(
    arg: SuggestionEntryArgument,
    update: SuggestionArgumentUpdate.Modify,
  ): Result<void> {
    const nameUpdate = this.applyPropertyUpdate('name', arg, update)
    if (!nameUpdate.ok) return nameUpdate
    const typeUpdate = this.applyFieldUpdate('reprType', update, (type) => {
      arg.reprType = type
    })
    if (!typeUpdate.ok) return typeUpdate
    const isSuspendedUpdate = this.applyPropertyUpdate('isSuspended', arg, update)
    if (!isSuspendedUpdate.ok) return isSuspendedUpdate
    const hasDefaultUpdate = this.applyPropertyUpdate('hasDefault', arg, update)
    if (!hasDefaultUpdate.ok) return hasDefaultUpdate
    this.applyOptPropertyUpdate('defaultValue', arg, update)
    return Ok()
  }

  private applyUpdate(
    entries: SuggestionDb,
    update: lsTypes.SuggestionsDatabaseUpdate,
  ): Result<void> {
    switch (update.type) {
      case 'Add': {
        return withContext(
          () => `when adding new entry ${JSON.stringify(update)}`,
          () => {
            const newEntry = this.entryFromLs(update.suggestion)
            if (!newEntry.ok) return newEntry
            entries.set(update.id, newEntry.value)
            return Ok()
          },
        )
      }
      case 'Remove': {
        if (!entries.delete(update.id)) {
          return Err(
            `Received "Remove" suggestion database update for non-existing id ${update.id}.`,
          )
        }
        return Ok()
      }
      case 'Modify': {
        return withContext(
          () => `when modifying entry to ${JSON.stringify(update)}`,
          () => {
            const entry = entries.get(update.id)
            if (entry == null) return Err(`Entry with id ${update.id} does not exist.`)
            return this.modifyEntry(entry, update)
          },
        )
      }
    }
  }

  private modifyEntry(
    entry: SuggestionEntry,
    update: SuggestionsDatabaseUpdate.Modify,
  ): Result<void> {
    for (const argumentUpdate of update.arguments ?? []) {
      const updateResult = this.applyArgumentsUpdate(entry.arguments, argumentUpdate)
      if (!updateResult.ok) return updateResult
    }

    const moduleUpdate = this.applyFieldUpdate('module', update, (module) =>
      this.setLsModule(entry, module),
    )
    if (!moduleUpdate.ok) return moduleUpdate
    if (moduleUpdate.value === false) return Err('Invalid module name')

    const selfTypeUpdate = this.applyFieldUpdate('selfType', update, (selfType) =>
      this.setLsSelfType(entry, selfType),
    )
    if (!selfTypeUpdate.ok) return selfTypeUpdate

    const returnTypeUpdate = this.applyFieldUpdate('returnType', update, (returnType) => {
      this.setLsReturnType(entry, returnType)
    })
    if (!returnTypeUpdate.ok) return returnTypeUpdate

    if (update.documentation != null) this.setLsDocumentation(entry, update.documentation.value)

    this.applyOptPropertyUpdate('scope', entry, update)

    if (update.reexport != null) {
      if (update.reexport.value != null) {
        const reexport = tryQualifiedName(update.reexport.value)
        if (!reexport.ok) return reexport
        entry.reexportedIn = reexport.value
      } else {
        delete entry.reexportedIn
      }
    }

    return Ok()
  }

  /** Update a suggestion database according to information provided by the language server. */
  applyUpdates(entries: SuggestionDb, updates: lsTypes.SuggestionsDatabaseUpdate[]) {
    for (const update of updates) {
      const updateResult = this.applyUpdate(entries, update)
      if (!updateResult.ok) {
        updateResult.error.log()
        if (entries.get(update.id) != null) {
          console.error(`Removing entry ${update.id}, because its state is unclear`)
          entries.delete(update.id)
        }
      }
    }
  }
}
