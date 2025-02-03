import { type ProjectNameStore } from '@/stores/projectNames'
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
import { type ProjectPath } from '@/util/projectPath'
import {
  Identifier,
  type IdentifierOrOperatorIdentifier,
  isIdentifierOrOperatorIdentifier,
  qnJoin,
  qnLastSegment,
} from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { type DeepReadonly, toValue } from 'vue'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import {
  SuggestionArgumentUpdate,
  SuggestionsDatabaseUpdate,
} from 'ydoc-shared/languageServerTypes/suggestions'

interface UpdateContext {
  groups: DeepReadonly<Group[]>
  projectNames: ProjectNameStore
}

abstract class BaseSuggestionEntry implements SuggestionEntryCommon {
  abstract readonly kind: SuggestionKind
  private documentationData: DocumentationData
  abstract name: IdentifierOrOperatorIdentifier
  abstract returnType(projectNames: ProjectNameStore): Typename

  protected constructor(
    documentation: string | undefined,
    public definedIn: ProjectPath,
    context: UpdateContext,
  ) {
    this.documentationData = documentationData(documentation, definedIn.project, context.groups)
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
    return this.definedIn.append(this.name)
  }

  setDocumentation(documentation: string | undefined, groups: DeepReadonly<Group[]>) {
    this.documentationData = documentationData(documentation, this.definedIn.project, groups)
  }
  setLsModule(lsModule: ProjectPath) {
    this.definedIn = lsModule
  }
  setLsReturnType(_returnType: Typename, _projectNames: ProjectNameStore): Result<void> {
    return Err(`Cannot modify \`returnType\` of entry type ${this.kind}.`)
  }
  setLsReexported(_reexported: ProjectPath | undefined): Result<void> {
    return Err(`Cannot modify \`reexported\` of entry type ${this.kind}.`)
  }
  setLsScope(_scope: lsTypes.SuggestionEntryScope | undefined): Result<void> {
    return Err(`Cannot modify \`scope\` of entry type ${this.kind}.`)
  }
}

class FunctionSuggestionEntryImpl extends BaseSuggestionEntry implements FunctionSuggestionEntry {
  readonly kind = SuggestionKind.Function
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    public scope: lsTypes.SuggestionEntryScope | undefined,
    args: lsTypes.SuggestionEntryArgument[],
    definedIn: ProjectPath,
    private lsReturnType: Typename,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
    this.arguments = args
  }

  returnType() {
    return this.lsReturnType
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Function,
    context: UpdateContext,
  ): Result<FunctionSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    return Ok(
      new FunctionSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.scope,
        lsEntry.arguments,
        module.value,
        lsEntry.returnType,
        lsEntry.documentation,
        context,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.lsReturnType = returnType
    return Ok()
  }
  override setLsScope(scope: lsTypes.SuggestionEntryScope | undefined) {
    this.scope = scope
    return Ok()
  }
}

class ModuleSuggestionEntryImpl extends BaseSuggestionEntry implements ModuleSuggestionEntry {
  readonly kind = SuggestionKind.Module

  private constructor(
    definedIn: ProjectPath,
    public reexportedIn: ProjectPath | undefined,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
  }

  get name() {
    return qnLastSegment(
      this.definedIn.normalized().path ??
        this.definedIn.normalized().project ??
        ('Main' as Identifier),
    )
  }
  returnType(projectNames: ProjectNameStore) {
    return projectNames.printProjectPath(this.definedIn)
  }
  override get definitionPath() {
    return this.definedIn
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Module,
    context: UpdateContext,
  ): Result<ModuleSuggestionEntry> {
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    const reexport = parseProjectPath(lsEntry, 'reexport', context)
    if (!reexport.ok) return reexport
    return Ok(
      new ModuleSuggestionEntryImpl(module.value, reexport.value, lsEntry.documentation, context),
    )
  }

  override setLsReexported(reexported: ProjectPath | undefined) {
    this.reexportedIn = reexported
    return Ok()
  }
}

class TypeSuggestionEntryImpl extends BaseSuggestionEntry implements TypeSuggestionEntry {
  readonly kind = SuggestionKind.Type
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    args: lsTypes.SuggestionEntryArgument[],
    public parentType: ProjectPath | undefined,
    definedIn: ProjectPath,
    public reexportedIn: ProjectPath | undefined,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
    this.arguments = args
  }

  returnType(projectNames: ProjectNameStore) {
    return qnJoin(projectNames.printProjectPath(this.definedIn), this.name)
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Type,
    context: UpdateContext,
  ): Result<TypeSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    const reexport = parseProjectPath(lsEntry, 'reexport', context)
    if (!reexport.ok) return reexport
    const parentType = parseProjectPath(lsEntry, 'parentType', context)
    if (!parentType.ok) return parentType
    return Ok(
      new TypeSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.params,
        lsEntry.parentType !== ANY_TYPE_QN ? parentType.value : undefined,
        module.value,
        reexport.value,
        lsEntry.documentation,
        context,
      ),
    )
  }

  override setLsReexported(reexported: ProjectPath | undefined) {
    this.reexportedIn = reexported
    return Ok()
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
    public reexportedIn: ProjectPath | undefined,
    public annotations: string[],
    definedIn: ProjectPath,
    public memberOf: ProjectPath,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
    this.arguments = args
  }

  returnType(projectNames: ProjectNameStore) {
    return projectNames.printProjectPath(this.memberOf)
  }
  override get definitionPath() {
    return this.memberOf.append(this.name)
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Constructor,
    context: UpdateContext,
  ): Result<ConstructorSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    const reexport = parseProjectPath(lsEntry, 'reexport', context)
    if (!reexport.ok) return reexport
    const returnType = parseProjectPath(lsEntry, 'returnType', context)
    if (!returnType.ok) return returnType
    return Ok(
      new ConstructorSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.arguments,
        reexport.value,
        lsEntry.annotations,
        module.value,
        returnType.value,
        lsEntry.documentation,
        context,
      ),
    )
  }

  override setLsReturnType(returnType: Typename, projectNames: ProjectNameStore) {
    const parsed = projectNames.parseProjectPathRaw(returnType)
    if (!parsed.ok) return parsed
    this.memberOf = parsed.value
    return Ok()
  }
  override setLsReexported(reexported: ProjectPath | undefined) {
    this.reexportedIn = reexported
    return Ok()
  }
}

class MethodSuggestionEntryImpl extends BaseSuggestionEntry implements MethodSuggestionEntry {
  readonly kind = SuggestionKind.Method
  arguments: lsTypes.SuggestionEntryArgument[]

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    args: lsTypes.SuggestionEntryArgument[],
    public reexportedIn: ProjectPath | undefined,
    public annotations: string[],
    private readonly isStatic: boolean,
    public memberOf: ProjectPath,
    definedIn: ProjectPath,
    private lsReturnType: Typename,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
    this.arguments = args
  }

  returnType() {
    return this.lsReturnType
  }
  override get definitionPath() {
    return this.memberOf.append(this.name)
  }
  get selfType() {
    return this.isStatic ? undefined : this.memberOf
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Method,
    context: UpdateContext,
  ): Result<MethodSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    const reexport = parseProjectPath(lsEntry, 'reexport', context)
    if (!reexport.ok) return reexport
    const selfType = parseProjectPath(lsEntry, 'selfType', context)
    if (!selfType.ok) return selfType
    return Ok(
      new MethodSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.arguments,
        reexport.value,
        lsEntry.annotations,
        lsEntry.isStatic,
        selfType.value,
        module.value,
        lsEntry.returnType,
        lsEntry.documentation,
        context,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.lsReturnType = returnType
    return Ok()
  }
  override setLsReexported(reexported: ProjectPath | undefined) {
    this.reexportedIn = reexported
    return Ok()
  }
  setLsSelfType(selfType: ProjectPath) {
    this.memberOf = selfType
  }
}

class LocalSuggestionEntryImpl extends BaseSuggestionEntry implements LocalSuggestionEntry {
  readonly kind = SuggestionKind.Local

  private constructor(
    readonly name: IdentifierOrOperatorIdentifier,
    public scope: lsTypes.SuggestionEntryScope | undefined,
    definedIn: ProjectPath,
    private lsReturnType: Typename,
    documentation: string | undefined,
    context: UpdateContext,
  ) {
    super(documentation, definedIn, context)
  }

  returnType() {
    return this.lsReturnType
  }

  static parse(
    lsEntry: lsTypes.SuggestionEntry.Local,
    context: UpdateContext,
  ): Result<LocalSuggestionEntry> {
    if (!isIdentifierOrOperatorIdentifier(lsEntry.name)) return Err('Invalid name')
    const module = parseProjectPath(lsEntry, 'module', context)
    if (!module.ok) return module
    return Ok(
      new LocalSuggestionEntryImpl(
        lsEntry.name,
        lsEntry.scope,
        module.value,
        lsEntry.returnType,
        lsEntry.documentation,
        context,
      ),
    )
  }

  override setLsReturnType(returnType: Typename) {
    this.lsReturnType = returnType
    return Ok()
  }
  override setLsScope(scope: lsTypes.SuggestionEntryScope | undefined) {
    this.scope = scope
    return Ok()
  }
}

function applyFieldUpdate<K extends string, T, R>(
  name: K,
  update: { [P in K]?: lsTypes.FieldUpdate<T> },
  updater: (newValue: T) => Result<R>,
): Result<Opt<R>> {
  const field = update[name]
  if (field == null) return Ok(null)
  return withContext(
    () => `when handling field "${name}" update`,
    () => {
      switch (field.tag) {
        case 'Set':
          if (field.value != null) {
            return updater(field.value)
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
    return Ok()
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
    return Ok()
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
  constructor(
    private readonly groups: ToValue<DeepReadonly<Group[]>>,
    private readonly projectNames: ProjectNameStore,
  ) {}

  /** Create a suggestion DB entry from data provided by the given language server. */
  entryFromLs(lsEntry: lsTypes.SuggestionEntry): Result<SuggestionEntry> {
    return withContext(
      () => `when creating entry`,
      (): Result<SuggestionEntry> => {
        const context = {
          groups: toValue(this.groups),
          projectNames: this.projectNames,
        }
        switch (lsEntry.type) {
          case 'function':
            return FunctionSuggestionEntryImpl.parse(lsEntry, context)
          case 'module':
            return ModuleSuggestionEntryImpl.parse(lsEntry, context)
          case 'type':
            return TypeSuggestionEntryImpl.parse(lsEntry, context)
          case 'constructor':
            return ConstructorSuggestionEntryImpl.parse(lsEntry, context)
          case 'method':
            return MethodSuggestionEntryImpl.parse(lsEntry, context)
          case 'local':
            return LocalSuggestionEntryImpl.parse(lsEntry, context)
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

    const moduleUpdate = applyFieldUpdate('module', update, (module) => {
      const pp = this.projectNames.parseProjectPathRaw(module)
      if (!pp.ok) return pp
      entry.setLsModule(pp.value)
      return Ok()
    })
    if (!moduleUpdate.ok) return moduleUpdate

    const selfTypeUpdate = applyFieldUpdate('selfType', update, (selfType) => {
      if (!(entry instanceof MethodSuggestionEntryImpl))
        return Err('Tried to update selfType in non-method entry')
      const pp = this.projectNames.parseProjectPathRaw(selfType)
      if (!pp.ok) return pp
      entry.setLsSelfType(pp.value)
      return Ok()
    })
    if (!selfTypeUpdate.ok) return selfTypeUpdate

    const returnTypeUpdate = applyFieldUpdate('returnType', update, (returnType) => {
      return entry.setLsReturnType(returnType, this.projectNames)
    })
    if (!returnTypeUpdate.ok) return returnTypeUpdate

    if (update.documentation)
      entry.setDocumentation(update.documentation.value, toValue(this.groups))

    if (update.scope) entry.setLsScope(update.scope.value)

    if (update.reexport) {
      const reexport = withContext(
        () => 'When parsing reexport field',
        () =>
          update.reexport?.value ?
            this.projectNames.parseProjectPathRaw(update.reexport.value)
          : Ok(undefined),
      )

      if (!reexport.ok) return reexport
      entry.setLsReexported(reexport.value)
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

function parseProjectPath<K extends string>(
  lsEntry: { [P in K]: string },
  field: K,
  context: UpdateContext,
): Result<ProjectPath>
function parseProjectPath<K extends string>(
  lsEntry: { [P in K]?: string },
  field: K,
  context: UpdateContext,
): Result<ProjectPath | undefined>
function parseProjectPath<K extends string>(
  lsEntry: { [P in K]?: string },
  field: K,
  context: UpdateContext,
) {
  return withContext(
    () => `Parsing ${field}`,
    () =>
      lsEntry[field] != null ?
        context.projectNames.parseProjectPathRaw(lsEntry[field])
      : Ok(undefined),
  )
}
