import type { PortId } from '@/providers/portInfo'
import { WidgetInput } from '@/providers/widgetRegistry'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import * as widgetCfg from '@/providers/widgetRegistry/configuration'
import type { SuggestionEntry, SuggestionEntryArgument } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { findLastIndex, tryGetIndex } from '@/util/data/array'
import { assert } from './assert'

export const enum ApplicationKind {
  Prefix,
  Infix,
}

/**
 * Information about an argument that doesn't have an assigned value yet, therefore are not
 * represented in the AST.
 */
export class ArgumentPlaceholder {
  constructor(
    public callId: string,
    public index: number,
    public argInfo: SuggestionEntryArgument,
    public kind: ApplicationKind,
    public insertAsNamed: boolean,
    public dynamicConfig?: WidgetConfiguration | undefined,
  ) {}

  static WithRetrievedConfig(
    callId: string,
    index: number,
    info: SuggestionEntryArgument,
    kind: ApplicationKind,
    insertAsNamed: boolean,
    functionCallConfig: widgetCfg.FunctionCall | undefined,
  ) {
    const cfg =
      info != null ? functionCallConfig?.parameters.get(info.name) ?? undefined : undefined
    return new ArgumentPlaceholder(callId, index, info, kind, insertAsNamed, cfg)
  }

  toWidgetInput(): WidgetInput {
    return {
      portId: this.portId,
      value: this.argInfo.defaultValue,
      expectedType: this.argInfo.reprType,
      [ArgumentInfoKey]: { info: this.argInfo, appKind: this.kind },
      dynamicConfig: this.dynamicConfig,
    }
  }

  get portId(): PortId {
    return `${this.callId}[${this.index}]` as PortId
  }
}

export class ArgumentAst {
  constructor(
    public ast: Ast.Ast,
    public index: number | undefined,
    public argInfo: SuggestionEntryArgument | undefined,
    public kind: ApplicationKind,
    public dynamicConfig?: WidgetConfiguration | undefined,
  ) {}

  static WithRetrievedConfig(
    ast: Ast.Ast,
    index: number | undefined,
    info: SuggestionEntryArgument | undefined,
    kind: ApplicationKind,
    functionCallConfig: widgetCfg.FunctionCall | undefined,
  ) {
    const cfg =
      info != null ? functionCallConfig?.parameters.get(info.name) ?? undefined : undefined
    return new ArgumentAst(ast, index, info, kind, cfg)
  }

  toWidgetInput(): WidgetInput {
    return {
      portId: this.portId,
      value: this.ast,
      expectedType: this.argInfo?.reprType,
      [ArgumentInfoKey]: { appKind: this.kind, info: this.argInfo },
      dynamicConfig: this.dynamicConfig,
    }
  }

  get portId(): PortId {
    return this.ast.id
  }
}

type InterpretedCall = InterpretedInfix | InterpretedPrefix

interface InterpretedInfix {
  kind: 'infix'
  appTree: Ast.OprApp
  operator: Ast.Token | undefined
  lhs: Ast.Ast | undefined
  rhs: Ast.Ast | undefined
}

interface InterpretedPrefix {
  kind: 'prefix'
  func: Ast.Ast
  args: FoundApplication[]
}

interface FoundApplication {
  appTree: Ast.App
  argument: Ast.Ast
  argName: string | undefined
}

export function interpretCall(callRoot: Ast.Ast, allowInterpretAsInfix: boolean): InterpretedCall {
  if (allowInterpretAsInfix && callRoot instanceof Ast.OprApp) {
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
  suggestion?: SuggestionEntry | undefined
  widgetCfg?: widgetCfg.FunctionCall | undefined
  subjectAsSelf?: boolean | undefined
}

export class ArgumentApplication {
  private constructor(
    public appTree: Ast.Ast,
    public target: ArgumentApplication | Ast.Ast | ArgumentPlaceholder | ArgumentAst,
    public infixOperator: Ast.Token | undefined,
    public argument: ArgumentAst | ArgumentPlaceholder,
  ) {}

  private static FromInterpretedInfix(interpreted: InterpretedInfix, callInfo: CallInfo) {
    const { suggestion, widgetCfg } = callInfo

    const kind = ApplicationKind.Infix
    const callId = interpreted.appTree.id

    const argFor = (key: 'lhs' | 'rhs', index: number) => {
      const tree = interpreted[key]
      const info = tryGetIndex(suggestion?.arguments, index) ?? unknownArgInfoNamed(key)
      return tree != null
        ? ArgumentAst.WithRetrievedConfig(tree, index, info, kind, widgetCfg)
        : ArgumentPlaceholder.WithRetrievedConfig(callId, index, info, kind, false, widgetCfg)
    }
    return new ArgumentApplication(
      interpreted.appTree,
      argFor('lhs', 0),
      interpreted.operator,
      argFor('rhs', 1),
    )
  }

  private static FromInterpretedPrefix(interpreted: InterpretedPrefix, callInfo: CallInfo) {
    const { notAppliedArguments, suggestion, widgetCfg, subjectAsSelf } = callInfo
    const callId = interpreted.func.id

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

    const notAppliedOriginally = new Set(notAppliedArguments ?? allPossiblePrefixArguments)
    const argumentsLeftToMatch = allPossiblePrefixArguments.filter((i) =>
      notAppliedOriginally.has(i),
    )

    const resolvedArgs: Array<{
      appTree: Ast.Ast
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
              argument: ArgumentPlaceholder.WithRetrievedConfig(
                callId,
                index,
                info,
                ApplicationKind.Prefix,
                placeholderAlreadyInserted,
                widgetCfg,
              ),
            })
            placeholderAlreadyInserted = true
          }
        }

        // Finally, we want to emit the named argument and remove it from the list of
        // remaining known params.
        const { index, info } = takeNamedArgumentFromDefinition(argumentInCode.argName) ?? {}
        resolvedArgs.push({
          appTree: argumentInCode.appTree,
          argument: ArgumentAst.WithRetrievedConfig(
            argumentInCode.argument,
            index,
            info ?? unknownArgInfoNamed(argumentInCode.argName),
            ApplicationKind.Prefix,
            widgetCfg,
          ),
        })
      } else {
        const argumentFromDefinition =
          argumentInCode.argName == null
            ? takeNextArgumentFromDefinition()
            : takeNamedArgumentFromDefinition(argumentInCode.argName)
        const { index, info } = argumentFromDefinition ?? {}
        resolvedArgs.push({
          appTree: argumentInCode.appTree,
          argument: ArgumentAst.WithRetrievedConfig(
            argumentInCode.argument,
            index,
            info ??
              (argumentInCode.argName != null
                ? unknownArgInfoNamed(argumentInCode.argName)
                : undefined),
            ApplicationKind.Prefix,
            widgetCfg,
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
        argument: ArgumentPlaceholder.WithRetrievedConfig(
          callId,
          index,
          info,
          ApplicationKind.Prefix,
          placeholderAlreadyInserted,
          widgetCfg,
        ),
      })
      placeholderAlreadyInserted = true
    }

    return resolvedArgs.reduce(
      (target: ArgumentApplication | Ast.Ast, toDisplay) =>
        new ArgumentApplication(toDisplay.appTree, target, undefined, toDisplay.argument),
      interpreted.func,
    )
  }

  static FromInterpretedWithInfo(
    interpreted: InterpretedCall,
    callInfo: CallInfo = {},
  ): ArgumentApplication | Ast.Ast {
    if (interpreted.kind === 'infix') {
      return ArgumentApplication.FromInterpretedInfix(interpreted, callInfo)
    } else {
      return ArgumentApplication.FromInterpretedPrefix(interpreted, callInfo)
    }
  }

  *iterApplications(): IterableIterator<ArgumentApplication> {
    let current: typeof this.target = this
    while (current instanceof ArgumentApplication) {
      yield current
      current = current.target
    }
  }

  toWidgetInput(): WidgetInput {
    return {
      portId:
        this.argument instanceof ArgumentAst
          ? this.appTree.id
          : (`app:${this.argument.portId}` as PortId),
      value: this.appTree,
      [ArgumentApplicationKey]: this,
    }
  }
}

const unknownArgInfoNamed = (name: string) => ({
  name,
  reprType: 'Any',
  isSuspended: false,
  hasDefault: false,
})

export function getAccessOprSubject(app: Ast.Ast): Ast.Ast | undefined {
  if (app instanceof Ast.PropertyAccess) return app.lhs
}

export const ArgumentApplicationKey: unique symbol = Symbol('ArgumentApplicationKey')
export const ArgumentInfoKey: unique symbol = Symbol('ArgumentInfoKey')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [ArgumentApplicationKey]?: ArgumentApplication
    [ArgumentInfoKey]?: {
      appKind: ApplicationKind
      info: SuggestionEntryArgument | undefined
    }
  }
}
