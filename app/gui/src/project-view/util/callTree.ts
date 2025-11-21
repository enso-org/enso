import { WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import type { WidgetConfiguration } from '$/providers/openedProjects/widgetRegistry/configuration'
import * as widgetCfg from '$/providers/openedProjects/widgetRegistry/configuration'
import { DisplayMode } from '$/providers/openedProjects/widgetRegistry/configuration'
import { syntheticPortId, type PortId } from '@/providers/portInfo'
// eslint-disable-next-line @typescript-eslint/no-unused-vars
import { type GraphDb, type MethodCallInfo } from '$/providers/openedProjects/graph/graphDatabase'
import type { GroupInfo } from '$/providers/openedProjects/suggestionDatabase'
import {
  isRequiredArgument,
  type CallableSuggestionEntry,
  type SuggestionEntryArgument,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { MethodSuggestionEntryImpl } from '$/providers/openedProjects/suggestionDatabase/lsUpdate'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { findLastIndex, tryGetIndex } from '@/util/data/array'
import type { DeepReadonly } from 'vue'
import { toValue } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'
import { assert } from './assert'
import type { MethodPointer } from './methodPointer'
import type { ToValue } from './reactivity'

export const enum ApplicationKind {
  Prefix,
  Infix,
}

class ArgumentFactory {
  constructor(
    private callId: AstId,
    private kind: ApplicationKind,
    private widgetCfg: widgetCfg.FunctionCall | undefined,
  ) {}

  placeholder(index: number, info: SuggestionEntryArgument, insertAsNamed: boolean) {
    return new ArgumentPlaceholder(
      this.callId,
      this.kind,
      this.dynamicConfig(info.name),
      index,
      info,
      insertAsNamed,
    )
  }

  argument(
    ast: Ast.Expression,
    index: number | undefined,
    info: SuggestionEntryArgument | undefined,
  ) {
    return new ArgumentAst(
      this.callId,
      this.kind,
      info && this.dynamicConfig(info.name),
      index,
      info,
      ast,
    )
  }

  private dynamicConfig(name: string) {
    return this.widgetCfg?.parameters.get(name) ?? undefined
  }
}

export type DynamicConfig = WidgetConfiguration & { display?: DisplayMode }
type WidgetInputValue = Ast.Expression | Ast.Token | string | undefined
abstract class Argument {
  protected constructor(
    public callId: AstId,
    public kind: ApplicationKind,
    public dynamicConfig: DynamicConfig | undefined,
    public index: number | undefined,
    public argInfo: SuggestionEntryArgument | undefined,
  ) {}

  abstract get portId(): PortId
  abstract get value(): WidgetInputValue

  get argId(): string | undefined {
    return this.argInfo && `${this.callId}[${this.argInfo.name}]`
  }

  get hideByDefault(): boolean {
    return false
  }

  toWidgetInput(): WidgetInput {
    return {
      portId: this.portId,
      value: this.value,
      expectedType: this.argInfo?.reprType,
      [ArgumentInfoKey]: { info: this.argInfo, appKind: this.kind, argId: this.argId },
      dynamicConfig: this.dynamicConfig,
    }
  }
}

/**
 * Information about an argument that doesn't have an assigned value yet, therefore are not
 * represented in the AST.
 */
export class ArgumentPlaceholder extends Argument {
  declare public index: number
  declare public argInfo: SuggestionEntryArgument
  /** TODO: Add docs */
  constructor(
    callId: AstId,
    kind: ApplicationKind,
    dynamicConfig: DynamicConfig | undefined,
    index: number,
    argInfo: SuggestionEntryArgument,
    public insertAsNamed: boolean,
  ) {
    super(callId, kind, dynamicConfig, index, argInfo)
  }

  /** TODO: Add docs */
  get portId(): PortId {
    return syntheticPortId(this.callId, this.index)
  }

  /** TODO: Add docs */
  get value(): WidgetInputValue {
    const value = this.argInfo.defaultValue
    return value == null || isRequiredArgument(this.argInfo) ? undefined : value
  }

  /** Whether the argument should be hidden when the component isn't currently focused for editing. */
  override get hideByDefault(): boolean {
    return (
      this.argInfo.hasDefault &&
      !isRequiredArgument(this.argInfo) &&
      this.dynamicConfig?.display !== DisplayMode.Always
    )
  }
}

/** TODO: Add docs */
export class ArgumentAst extends Argument {
  /** TODO: Add docs */
  constructor(
    callId: AstId,
    kind: ApplicationKind,
    dynamicConfig: DynamicConfig | undefined,
    index: number | undefined,
    argInfo: SuggestionEntryArgument | undefined,
    public ast: Ast.Expression,
  ) {
    super(callId, kind, dynamicConfig, index, argInfo)
  }

  /** TODO: Add docs */
  get portId(): PortId {
    return this.ast.id
  }

  /** TODO: Add docs */
  get value() {
    return this.ast
  }
}

export type InterpretedCall = InterpretedInfix | InterpretedPrefix

interface InterpretedInfix {
  kind: 'infix'
  appTree: Ast.OprApp
  operator: Ast.Token | undefined
  lhs: Ast.Expression | undefined
  rhs: Ast.Expression | undefined
}

interface InterpretedPrefix {
  kind: 'prefix'
  func: Ast.Expression
  args: FoundApplication[]
}

interface FoundApplication {
  appTree: Ast.App
  argument: Ast.Expression
  argName: string | undefined
}

/** TODO: Add docs */
export function interpretCall(callRoot: Ast.Expression): InterpretedCall {
  if (callRoot instanceof Ast.OprApp) {
    // Infix chains are handled one level at a time. Each application may have at most 2 arguments.
    return {
      kind: 'infix',
      appTree: callRoot,
      operator: callRoot.operator.ok ? callRoot.operator.value : undefined,
      lhs: callRoot.lhs ?? undefined,
      rhs: callRoot.rhs ?? undefined,
    }
  } else {
    // Prefix chains are handled all at once, as they may have arbitrary number of arguments.
    const foundApplications: FoundApplication[] = []
    let nextApplication = callRoot
    // Traverse the AST and find all arguments applied in sequence to the same function.
    while (nextApplication instanceof Ast.App) {
      foundApplications.push({
        appTree: nextApplication,
        argument: nextApplication.argument,
        argName: nextApplication.argumentName?.code() ?? undefined,
      })
      nextApplication = nextApplication.function
    }
    return {
      kind: 'prefix',
      func: nextApplication,
      // The applications are peeled away from outer to inner, so arguments are in reverse order. We
      // need to reverse them back to match them with the order in suggestion entry.
      args: foundApplications.reverse(),
    }
  }
}

interface CallInfo {
  notAppliedArguments?: number[] | undefined
  suggestion?: CallableSuggestionEntry | undefined
  widgetCfg?: widgetCfg.FunctionCall | undefined
  subjectAsSelf?: boolean | undefined
  suppressPlaceholders?: boolean | undefined
}

/** TODO: Add docs */
export class ArgumentApplication {
  private constructor(
    public appTree: Ast.Expression,
    public target: ArgumentApplication | Ast.Expression | ArgumentPlaceholder | ArgumentAst,
    public infixOperator: Ast.Token | undefined,
    public argument: ArgumentAst | ArgumentPlaceholder,
    public calledFunction: CallableSuggestionEntry | undefined,
    public isInnermost: boolean,
  ) {}

  private static FromInterpretedInfix(interpreted: InterpretedInfix, callInfo: CallInfo) {
    const { suggestion, widgetCfg } = callInfo

    const makeArg = new ArgumentFactory(interpreted.appTree.id, ApplicationKind.Infix, widgetCfg)
    const argFor = (key: 'lhs' | 'rhs', index: number) => {
      const tree = interpreted[key]
      const info = tryGetIndex(suggestion?.arguments, index) ?? unknownArgInfoNamed(key)
      return tree != null ?
          makeArg.argument(tree, index, info)
        : makeArg.placeholder(index, info, false)
    }
    return new ArgumentApplication(
      interpreted.appTree,
      argFor('lhs', 0),
      interpreted.operator,
      argFor('rhs', 1),
      suggestion,
      true,
    )
  }

  private static FromInterpretedPrefix(interpreted: InterpretedPrefix, callInfo: CallInfo) {
    const { notAppliedArguments, suggestion, widgetCfg, subjectAsSelf, suppressPlaceholders } =
      callInfo

    const knownArguments = suggestion?.arguments
    const allPossiblePrefixArguments = Array.from(knownArguments ?? [], (_, i) => i)

    // when this is a method application with applied 'self', the subject of the access operator is
    // treated as a 'self' argument.
    if (
      subjectAsSelf &&
      knownArguments?.[0]?.name === 'self' &&
      getAccessOprSubject(interpreted.func) != null
    ) {
      allPossiblePrefixArguments.shift()
    }

    const notAppliedOriginally = new Set(
      suppressPlaceholders ? [] : (notAppliedArguments ?? allPossiblePrefixArguments),
    )
    const argumentsLeftToMatch = allPossiblePrefixArguments.filter((i) =>
      notAppliedOriginally.has(i),
    )

    const resolvedArgs: Array<{
      appTree: Ast.Expression
      argument: ArgumentAst | ArgumentPlaceholder
    }> = []

    function nextArgumentNameInDefinition() {
      return tryGetIndex(knownArguments, argumentsLeftToMatch[0])?.name
    }

    function takeNextArgumentFromDefinition() {
      const index = argumentsLeftToMatch.shift()
      const info = tryGetIndex(knownArguments, index)
      return index != null && info != null ? { index, info } : undefined
    }

    function takeNamedArgumentFromDefinition(name: string) {
      const takeIdx = argumentsLeftToMatch.findIndex(
        (id) => tryGetIndex(knownArguments, id)?.name === name,
      )
      const index = argumentsLeftToMatch.splice(takeIdx, 1)[0]
      const info = tryGetIndex(knownArguments, index)
      return index != null && info != null ? { index, info } : undefined
    }

    function putBackArgument(index: number) {
      argumentsLeftToMatch.unshift(index)
    }

    const lastPositionalArgIndex = findLastIndex(interpreted.args, (arg) => arg.argName == null)

    let placeholderAlreadyInserted = false

    let nextArgDefinition: ReturnType<typeof takeNextArgumentFromDefinition>

    const makeArg = new ArgumentFactory(interpreted.func.id, ApplicationKind.Prefix, widgetCfg)

    // Always insert a placeholder for the missing argument at the first position that is legal
    // and don't invalidate further positional arguments, treating the named arguments at correct
    // position as if they were positional.
    for (let position = 0; position < interpreted.args.length; ++position) {
      const argumentInCode = interpreted.args[position]
      assert(!!argumentInCode)
      const pastPositionalArguments = position > (lastPositionalArgIndex ?? -1)

      if (
        pastPositionalArguments &&
        argumentInCode.argName != null &&
        argumentInCode.argName !== nextArgumentNameInDefinition()
      ) {
        // Named argument that is not in its natural position, and there are no more
        // positional arguments to emit in the chain. At this point placeholders can be
        // inserted. We need to figure out which placeholders can be inserted before
        // emitting this named argument.

        // all remaining arguments must be named, as we are past all positional arguments.
        const remainingAppliedArguments = interpreted.args.slice(position)

        // For each subsequent argument in its current natural position, insert a
        // placeholder. Do that only if the argument is not defined further in the chain.
        while ((nextArgDefinition = takeNextArgumentFromDefinition())) {
          const { index, info } = nextArgDefinition
          const isAppliedFurther = remainingAppliedArguments.some(
            (arg) => arg.argName === info.name,
          )
          if (isAppliedFurther) {
            putBackArgument(index)
            break
          } else {
            resolvedArgs.push({
              appTree: argumentInCode.appTree.function,
              argument: makeArg.placeholder(index, info, placeholderAlreadyInserted),
            })
            placeholderAlreadyInserted = true
          }
        }

        // Finally, we want to emit the named argument and remove it from the list of
        // remaining known params.
        const { index, info } = takeNamedArgumentFromDefinition(argumentInCode.argName) ?? {}
        resolvedArgs.push({
          appTree: argumentInCode.appTree,
          argument: makeArg.argument(
            argumentInCode.argument,
            index,
            info ?? unknownArgInfoNamed(argumentInCode.argName),
          ),
        })
      } else {
        const argumentFromDefinition =
          argumentInCode.argName == null ?
            takeNextArgumentFromDefinition()
          : takeNamedArgumentFromDefinition(argumentInCode.argName)
        const { index, info } = argumentFromDefinition ?? {}
        resolvedArgs.push({
          appTree: argumentInCode.appTree,
          argument: makeArg.argument(
            argumentInCode.argument,
            index,
            info ??
              (argumentInCode.argName != null ?
                unknownArgInfoNamed(argumentInCode.argName)
              : undefined),
          ),
        })
      }
    }

    const outerApp = interpreted.args[interpreted.args.length - 1]?.appTree ?? interpreted.func
    // If there are any remaining known parameters, they must be inserted as trailing placeholders.
    while ((nextArgDefinition = takeNextArgumentFromDefinition())) {
      const { index, info } = nextArgDefinition
      resolvedArgs.push({
        appTree: outerApp,
        argument: makeArg.placeholder(index, info, placeholderAlreadyInserted),
      })
      placeholderAlreadyInserted = true
    }

    return resolvedArgs.reduce(
      (target: ArgumentApplication | Ast.Expression, toDisplay) =>
        new ArgumentApplication(
          toDisplay.appTree,
          target,
          undefined,
          toDisplay.argument,
          suggestion,
          target === interpreted.func,
        ),
      interpreted.func,
    )
  }

  /** TODO: Add docs */
  static FromInterpretedWithInfo(
    interpreted: InterpretedCall,
    callInfo: CallInfo = {},
  ): ArgumentApplication | Ast.Expression {
    if (interpreted.kind === 'infix') {
      return ArgumentApplication.FromInterpretedInfix(interpreted, callInfo)
    } else {
      return ArgumentApplication.FromInterpretedPrefix(interpreted, callInfo)
    }
  }

  /** TODO: Add docs */
  *iterApplications(): IterableIterator<ArgumentApplication> {
    // This is not an alias, as it's an iteration variable.
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let current: typeof this.target = this
    while (current instanceof ArgumentApplication) {
      yield current
      current = current.target
    }
  }

  /** TODO: Add docs */
  toWidgetInput(): WidgetInput {
    return {
      portId:
        this.argument instanceof ArgumentAst ?
          this.appTree.id
        : syntheticPortId(this.argument.portId, ':app:'),
      value: this.appTree,
      [ArgumentApplicationKey]: this,
    }
  }

  /** TODO: Add docs */
  static collectArgumentNamesAndUuids(
    value: InterpretedCall,
    mci: MethodCallInfo | undefined,
  ): Record<string, ExternalId> {
    const namesAndExternalIds: Array<{
      name: string | null
      uuid: ExternalId | undefined
    }> = []

    const args = ArgumentApplication.FromInterpretedWithInfo(value)
    if (args instanceof ArgumentApplication) {
      for (const n of args.iterApplications()) {
        const a = n.argument
        if (a instanceof ArgumentPlaceholder) {
          // pass thru
        } else {
          namesAndExternalIds.push({
            name: a.argInfo?.name.toString() ?? null,
            uuid: a.ast.externalId,
          })
        }
      }
    } else {
      // don't process
    }
    namesAndExternalIds.reverse()

    const argsExternalIds: Record<string, ExternalId> = {}
    let index = 'self' === mci?.suggestion.arguments[0]?.name ? 1 : 0
    for (const { uuid } of namesAndExternalIds) {
      const notApplied = mci?.methodCall.notAppliedArguments ?? []
      while (notApplied.indexOf(index) != -1) {
        index++
      }
      if (uuid) {
        argsExternalIds['' + index] = uuid
        const suggestedName: string | undefined = mci?.suggestion.arguments[index]?.name
        if (suggestedName) {
          argsExternalIds[suggestedName] = uuid
        }
      }
      index++
    }
    for (const { name, uuid } of namesAndExternalIds) {
      if (name && uuid) {
        argsExternalIds[name] = uuid
      }
    }
    return argsExternalIds
  }
}

const unknownArgInfoNamed = (name: string) => ({
  name,
  reprType: 'Any',
  isSuspended: false,
  hasDefault: false,
})

/** TODO: Add docs */
export function getAccessOprSubject(app: Ast.Expression | undefined): Ast.Expression | undefined {
  if (app instanceof Ast.PropertyAccess) return app.lhs
}

/**
 * Same as {@link GraphDb.getMethodCallInfo} but with a special handling for nested Applications.
 * Sometimes we receive MethodCallInfo for inner sub-applications of the expression,
 * and we want to reuse it for outer application expressions when adding new arguments to the call.
 * It requires adjusting `notAppliedArguments` array, but otherwise is a straightforward recursive call.
 * We stop recursion at any not-application AST. We expect that a subexpression’s info is only valid if it is a part of the prefix application chain.
 * We also don’t consider infix applications here, as using them inside a prefix chain would require additional syntax (like parenthesis).
 */
export function getMethodCallInfoRecursively(
  ast: Ast.Expression,
  graphDb: { getMethodCallInfo(id: AstId): MethodCallInfo | undefined },
): MethodCallInfo | undefined {
  const topLevelAst = ast
  for (;;) {
    const info = graphDb.getMethodCallInfo(ast.id)
    if (info) {
      // There is an info available! Stop the recursion and adjust `notAppliedArguments`.
      // Indices of all named arguments applied so far.
      return {
        methodCall: {
          ...info.methodCall,
          notAppliedArguments: filterNotAppliedArguments(
            getCallAppliedArguments(topLevelAst),
            info.suggestion.arguments,
            info.methodCall.notAppliedArguments,
          ),
        },
        methodCallSource: ast.id,
        suggestion: info.suggestion,
      }
    }
    // No info, continue recursion to the next sub-application AST.
    if (ast instanceof Ast.App) ast = ast.function
    else break
  }
}

/**
 * Derive a synthetic method call info from locally available module code.
 * Potentially integrate non-derivable data from language server response, if available.
 */
export function deriveLocalCallInfoFromCode(
  methodPointer: MethodPointer,
  call: Ast.Expression,
  groups: ToValue<DeepReadonly<GroupInfo[]>>,
): MethodCallInfo | undefined {
  const moduleRoot = call.module.root()
  if (!moduleRoot) return
  const methodAst = Ast.findModuleMethod(moduleRoot, methodPointer.name)
  if (!methodAst) return
  const suggestion = deriveLocalMethodSuggestion(
    methodPointer,
    methodAst.statement,
    toValue(groups),
  )
  const appliedInfo = getCallAppliedArguments(call)
  const notAppliedArguments = filterNotAppliedArguments(appliedInfo, suggestion.arguments)
  return {
    suggestion,
    methodCallSource: call.id,
    methodCall: { methodPointer, notAppliedArguments },
  }
}

function deriveLocalMethodSuggestion(
  methodPointer: MethodPointer,
  ast: Ast.FunctionDef,
  groups: DeepReadonly<GroupInfo[]> = [],
): CallableSuggestionEntry {
  const args: SuggestionEntryArgument[] = ast.argumentDefinitions.map((def) => {
    return {
      name: def.pattern.node.code(),
      reprType: def.type?.type.node.code() ?? 'Any',
      isSuspended: def.suspension != null,
      hasDefault: def.defaultValue != null,
      defaultValue: def.defaultValue?.expression.node.code() ?? null,
    } satisfies SuggestionEntryArgument
  })
  const annotations = ast.annotations.map((ann) => ann.annotation.node.code())
  const returnType = ast.returnType?.code() ?? 'Any'
  return MethodSuggestionEntryImpl.synthesizeLocal(
    methodPointer,
    args,
    returnType,
    annotations,
    groups,
  )
}

interface AppliedArgsInfo {
  positional: number
  named: string[]
}

function getCallAppliedArguments(ast: Ast.Expression) {
  const named: string[] = []
  let positional = 0
  while (ast instanceof Ast.App) {
    if (ast.argumentName) {
      named.push(ast.argumentName.code())
    } else {
      positional += 1
    }
    ast = ast.function
  }
  return { positional, named }
}

function filterNotAppliedArguments(
  appliedInfo: AppliedArgsInfo,
  methodArgs: SuggestionEntryArgument[],
  notApplied: number[] = methodArgs.map((_, i) => i),
) {
  // There is an info available! Stop the recursion and adjust `notAppliedArguments`.
  // Indices of all named arguments applied so far.
  const appliedNamed =
    appliedInfo.named.length > 0 ?
      methodArgs
        .map((arg, index) => (appliedInfo.named.includes(arg.name) ? index : -1))
        .filter((i) => i !== -1)
    : []

  const withoutNamed = notApplied.filter((idx) => !appliedNamed.includes(idx))
  return withoutNamed.sort().slice(appliedInfo.positional)
}

export const ArgumentApplicationKey: unique symbol = Symbol.for('WidgetInput:ArgumentApplication')
export const ArgumentInfoKey: unique symbol = Symbol.for('WidgetInput:ArgumentInfo')
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [ArgumentApplicationKey]?: ArgumentApplication
    [ArgumentInfoKey]?: {
      appKind: ApplicationKind
      info: SuggestionEntryArgument | undefined
      argId: string | undefined
    }
  }
}
