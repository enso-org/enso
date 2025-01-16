import { type Group, SuggestionDb } from '@/stores/suggestionDatabase'
import {
  documentationData,
  type DocumentationData,
} from '@/stores/suggestionDatabase/documentation'
import {
  type ConstructorSuggestionEntry,
  type FunctionSuggestionEntry,
  type LocalSuggestionEntry,
  type MethodSuggestionEntry,
  type ModuleSuggestionEntry,
  type SuggestionEntry,
  type SuggestionEntryArgument,
  type SuggestionEntryCommon,
  SuggestionKind,
  type Typename,
  type TypeSuggestionEntry,
} from '@/stores/suggestionDatabase/entry'
import { assert, assertNever } from '@/util/assert'
import { type Opt } from '@/util/data/opt'
import { Err, Ok, type Result, withContext } from '@/util/data/result'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import {
  type IdentifierOrOperatorIdentifier,
  isIdentifierOrOperatorIdentifier,
  isQualifiedName,
  normalizeQualifiedName,
  qnJoin,
  qnLastSegment,
  type QualifiedName,
  tryQualifiedName,
} from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { type DeepReadonly, toValue } from 'vue'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import {
  SuggestionArgumentUpdate,
  SuggestionsDatabaseUpdate,
} from 'ydoc-shared/languageServerTypes/suggestions'

function isOptQN(optQn: string | undefined): optQn is QualifiedName | undefined {
  if (optQn == null) return true
  return isQualifiedName(optQn)
}

abstract class BaseSuggestionEntry implements SuggestionEntryCommon {
  abstract readonly kind: SuggestionKind
  private documentationData: DocumentationData
  abstract name: IdentifierOrOperatorIdentifier
  abstract returnType: Typename

  protected constructor(
    documentation: string | undefined,
    public definedIn: QualifiedName,
    groups: DeepReadonly<Group[]>,
  ) {
    this.documentationData = documentationData(documentation, definedIn, groups)
  }

  get documentation() {
    return this.documentationData.documentation
  }
  get aliases() {
    return this.documentationData.aliases
  }
  get iconName() {
    return this.documentationData.iconName
  }
  get groupIndex() {
    return this.documentationData.groupIndex
  }
  get isPrivate() {
    return this.documentationData.isPrivate
  }
  get isUnstable() {
    return this.documentationData.isUnstable
  }
  get definitionPath() {
    return qnJoin(this.definedIn, this.name)
  }

  setDocumentation(documentation: string | undefined, groups: DeepReadonly<Group[]>) {
    this.documentationData = documentationData(documentation, this.definedIn, groups)
  }
  setLsModule(lsModule: string) {
    const qn = tryQualifiedName(lsModule)
    if (!qn.ok) return false
    this.definedIn = normalizeQualifiedName(qn.value)
    return true
  }
  setLsReturnType(_returnType: Typename) {
    console.warn(`Cannot modify \`returnType\` of entry type ${this.kind}.`)
  }
  setLsReexported(_reexported: QualifiedName | undefined) {
    console.warn(`Cannot modify \`reexported\` of entry type ${this.kind}.`)
  }
  setLsScope(_scope: lsTypes.SuggestionEntryScope | undefined) {
    console.warn(`Cannot modify \`scope\` of entry type ${this.kind}.`)
  }
}

class FunctionSuggestionEntryImpl extends BaseSuggestionEntry implements FunctionSuggestionEntry {
  readonly kind = SuggestionKind.Function
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    public scope: lsTypes.SuggestionEntryScope | undefined,
    args: lsTypes.SuggestionEntryArgument[],
    definedIn: QualifiedName,
    public returnType: Typename,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
    this.arguments = args
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Function,
    groups: DeepReadonly<Group[]>,
  ): Result<FunctionSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    return Ok(
      new FunctionSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.scope,
        lsEntry.arguments,
        normalizeQualifiedName(lsEntry.module),
        lsEntry.returnType,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.returnType = returnType
  }
  override setLsScope(scope: lsTypes.SuggestionEntryScope | undefined) {
    this.scope = scope
  }
}

class ModuleSuggestionEntryImpl extends BaseSuggestionEntry implements ModuleSuggestionEntry {
  readonly kind = SuggestionKind.Module

  private constructor(
    definedIn: QualifiedName,
    public reexportedIn: QualifiedName | undefined,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
  }

  get name() {
    return qnLastSegment(this.definedIn)
  }
  get returnType() {
    return this.definedIn
  }
  override get definitionPath() {
    return this.definedIn
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Module,
    groups: DeepReadonly<Group[]>,
  ): Result<ModuleSuggestionEntry> {
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    if (!isOptQN(lsEntry.reexport)) return Err('Invalid reexport')
    return Ok(
      new ModuleSuggestionEntryImpl(
        normalizeQualifiedName(lsEntry.module),
        lsEntry.reexport,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReexported(reexported: QualifiedName | undefined) {
    this.reexportedIn = reexported
  }
}

class TypeSuggestionEntryImpl extends BaseSuggestionEntry implements TypeSuggestionEntry {
  readonly kind = SuggestionKind.Type
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    args: lsTypes.SuggestionEntryArgument[],
    public parentType: QualifiedName | undefined,
    definedIn: QualifiedName,
    public reexportedIn: QualifiedName | undefined,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
    this.arguments = args
  }

  get returnType() {
    return qnJoin(this.definedIn, this.name)
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Type,
    groups: DeepReadonly<Group[]>,
  ): Result<TypeSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    if (!isOptQN(lsEntry.reexport)) return Err('Invalid reexport')
    if (!isOptQN(lsEntry.parentType)) return Err('Invalid parent type')
    return Ok(
      new TypeSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.params,
        lsEntry.parentType === ANY_TYPE_QN ? undefined : lsEntry.parentType,
        normalizeQualifiedName(lsEntry.module),
        lsEntry.reexport,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReexported(reexported: QualifiedName | undefined) {
    this.reexportedIn = reexported
  }
}

class ConstructorSuggestionEntryImpl
  extends BaseSuggestionEntry
  implements ConstructorSuggestionEntry
{
  readonly kind = SuggestionKind.Constructor
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    args: lsTypes.SuggestionEntryArgument[],
    public reexportedIn: QualifiedName | undefined,
    public annotations: string[],
    definedIn: QualifiedName,
    public returnType: Typename,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
    this.arguments = args
  }

  get memberOf() {
    const qn = tryQualifiedName(this.returnType)
    return qn.ok ? normalizeQualifiedName(qn.value) : undefined
  }
  override get definitionPath() {
    return this.memberOf ? qnJoin(this.memberOf, this.name) : super.definitionPath
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Constructor,
    groups: DeepReadonly<Group[]>,
  ): Result<ConstructorSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    if (!isOptQN(lsEntry.reexport)) return Err('Invalid reexport')
    return Ok(
      new ConstructorSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.arguments,
        lsEntry.reexport,
        lsEntry.annotations,
        normalizeQualifiedName(lsEntry.module),
        lsEntry.returnType,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.returnType = returnType
  }
  override setLsReexported(reexported: QualifiedName | undefined) {
    this.reexportedIn = reexported
  }
}

class MethodSuggestionEntryImpl extends BaseSuggestionEntry implements MethodSuggestionEntry {
  readonly kind = SuggestionKind.Method
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    args: lsTypes.SuggestionEntryArgument[],
    public reexportedIn: QualifiedName | undefined,
    public annotations: string[],
    private readonly isStatic: boolean,
    private lsSelfType: string,
    definedIn: QualifiedName,
    public returnType: Typename,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
    this.arguments = args
  }

  get memberOf() {
    const qn = tryQualifiedName(this.lsSelfType)
    return qn.ok ? normalizeQualifiedName(qn.value) : undefined
  }
  override get definitionPath() {
    return this.memberOf ? qnJoin(this.memberOf, this.name) : super.definitionPath
  }
  get selfType() {
    if (this.isStatic) return undefined
    return this.lsSelfType
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Method,
    groups: DeepReadonly<Group[]>,
  ): Result<MethodSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    if (!isOptQN(lsEntry.reexport)) return Err('Invalid reexport')
    return Ok(
      new MethodSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.arguments,
        lsEntry.reexport,
        lsEntry.annotations,
        lsEntry.isStatic,
        lsEntry.selfType,
        normalizeQualifiedName(lsEntry.module),
        lsEntry.returnType,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.returnType = returnType
  }
  override setLsReexported(reexported: QualifiedName | undefined) {
    this.reexportedIn = reexported
  }
  setLsSelfType(selfType: string) {
    this.lsSelfType = selfType
  }
}

class LocalSuggestionEntryImpl extends BaseSuggestionEntry implements LocalSuggestionEntry {
  readonly kind = SuggestionKind.Local

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    public scope: lsTypes.SuggestionEntryScope | undefined,
    definedIn: QualifiedName,
    public returnType: Typename,
    documentation: string | undefined,
    groups: DeepReadonly<Group[]>,
  ) {
    super(documentation, definedIn, groups)
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Local,
    groups: DeepReadonly<Group[]>,
  ): Result<LocalSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    if (!isQualifiedName(lsEntry.module)) return Err('Invalid module name')
    return Ok(
      new LocalSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.scope,
        normalizeQualifiedName(lsEntry.module),
        lsEntry.returnType,
        lsEntry.documentation,
        groups,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.returnType = returnType
  }
  override setLsScope(scope: lsTypes.SuggestionEntryScope | undefined) {
    this.scope = scope
  }
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
): Result<void> {
  const apply = applyFieldUpdate(name, update, (newValue) => {
    obj[name] = newValue
  })
  if (!apply.ok) return apply
  return Ok()
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
          return modifyArgument(arg, update)
        },
      )
    }
  }
}

function modifyArgument(
  arg: SuggestionEntryArgument,
  update: SuggestionArgumentUpdate.Modify,
): Result<void> {
  const nameUpdate = applyPropertyUpdate('name', arg, update)
  if (!nameUpdate.ok) return nameUpdate
  const typeUpdate = applyFieldUpdate('reprType', update, (type) => {
    arg.reprType = type
  })
  if (!typeUpdate.ok) return typeUpdate
  const isSuspendedUpdate = applyPropertyUpdate('isSuspended', arg, update)
  if (!isSuspendedUpdate.ok) return isSuspendedUpdate
  const hasDefaultUpdate = applyPropertyUpdate('hasDefault', arg, update)
  if (!hasDefaultUpdate.ok) return hasDefaultUpdate
  applyOptPropertyUpdate('defaultValue', arg, update)
  return Ok()
}

/** Interprets language server messages to create and update suggestion database entries. */
export class SuggestionUpdateProcessor {
  /** Constructor. */
  constructor(private readonly groups: ToValue<DeepReadonly<Group[]>>) {}

  /** Create a suggestion DB entry from data provided by the given language server. */
  entryFromLs(lsEntry: lsTypes.SuggestionEntry): Result<SuggestionEntry> {
    return withContext(
      () => `when creating entry`,
      (): Result<SuggestionEntry> => {
        const groups = toValue(this.groups)
        switch (lsEntry.type) {
          case 'function':
            return FunctionSuggestionEntryImpl.parse(lsEntry, groups)
          case 'module':
            return ModuleSuggestionEntryImpl.parse(lsEntry, groups)
          case 'type':
            return TypeSuggestionEntryImpl.parse(lsEntry, groups)
          case 'constructor':
            return ConstructorSuggestionEntryImpl.parse(lsEntry, groups)
          case 'method':
            return MethodSuggestionEntryImpl.parse(lsEntry, groups)
          case 'local':
            return LocalSuggestionEntryImpl.parse(lsEntry, groups)
          default:
            assertNever(lsEntry)
        }
      },
    )
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
    assert(entry instanceof BaseSuggestionEntry)

    if ('arguments' in entry) {
      for (const argumentUpdate of update.arguments ?? []) {
        const updateResult = applyArgumentsUpdate(entry.arguments, argumentUpdate)
        if (!updateResult.ok) return updateResult
      }
    }

    const moduleUpdate = applyFieldUpdate('module', update, (module) => entry.setLsModule(module))
    if (!moduleUpdate.ok) return moduleUpdate
    if (moduleUpdate.value === false) return Err('Invalid module name')

    const selfTypeUpdate = applyFieldUpdate('selfType', update, (selfType) => {
      if (!(entry instanceof MethodSuggestionEntryImpl)) return false
      entry.setLsSelfType(selfType)
    })
    if (!selfTypeUpdate.ok) return selfTypeUpdate

    const returnTypeUpdate = applyFieldUpdate('returnType', update, (returnType) => {
      entry.setLsReturnType(returnType)
    })
    if (!returnTypeUpdate.ok) return returnTypeUpdate

    if (update.documentation)
      entry.setDocumentation(update.documentation.value, toValue(this.groups))

    if (update.scope) entry.setLsScope(update.scope.value)

    if (update.reexport) {
      if (!isOptQN(update.reexport.value)) return Err('Invalid reexport')
      entry.setLsReexported(update.reexport.value)
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
