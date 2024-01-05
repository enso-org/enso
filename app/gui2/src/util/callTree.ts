import type { PortId } from '@/providers/portInfo'
import { WidgetInput } from '@/providers/widgetRegistry'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import * as widgetCfg from '@/providers/widgetRegistry/configuration'
import type { SuggestionEntry, SuggestionEntryArgument } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { tryGetIndex } from '@/util/data/array'
import type { MethodCall } from 'shared/languageServerTypes'

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
    return this.ast.exprId
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
  noArgsCall?: MethodCall | undefined
  appMethodCall?: MethodCall | undefined
  suggestion?: SuggestionEntry | undefined
  widgetCfg?: widgetCfg.FunctionCall | undefined
}

export class ArgumentApplication {
  private constructor(
    public appTree: Ast.Ast,
    public target: ArgumentApplication | Ast.Ast | ArgumentPlaceholder | ArgumentAst,
    public infixOperator: Ast.Token | undefined,
    public argument: ArgumentAst | ArgumentPlaceholder,
  ) {}

  private static FromInterpretedInfix(interpreted: InterpretedInfix, callInfo: CallInfo) {
    // Access infixes are not real infix calls.
    if (isAccessOperator(interpreted.operator)) return interpreted.appTree
    const { suggestion, widgetCfg } = callInfo

    const kind = ApplicationKind.Infix
    const callId = interpreted.appTree.exprId

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

  private static FromInterpretedPrefix(
    interpreted: InterpretedPrefix,
    callInfo: CallInfo,
    stripSelfArgument: boolean,
  ) {
    const { noArgsCall, appMethodCall, suggestion, widgetCfg } = callInfo
    const kind = ApplicationKind.Prefix
    const callId = interpreted.func.exprId

    const knownArguments = suggestion?.arguments
    const notAppliedArguments = appMethodCall?.notAppliedArguments ?? []
    const placeholdersToInsert = notAppliedArguments.slice()
    const notAppliedSet = new Set(notAppliedArguments)
    const allPossiblePrefixArguments = Array.from(knownArguments ?? [], (_, i) => i)

    // when this is a method application with applied 'self', the subject of the access operator is
    // treated as a 'self' argument.
    if (
      stripSelfArgument &&
      knownArguments?.[0]?.name === 'self' &&
      getAccessOprSubject(interpreted.func) != null
    ) {
      allPossiblePrefixArguments.shift()
    }
    const notAppliedOriginally = new Set(
      noArgsCall?.notAppliedArguments ?? allPossiblePrefixArguments,
    )
    const argumentsLeftToMatch = allPossiblePrefixArguments.filter(
      (i) => !notAppliedSet.has(i) && notAppliedOriginally.has(i),
    )

    const prefixArgsToDisplay: Array<{
      appTree: Ast.Ast
      argument: ArgumentAst | ArgumentPlaceholder
    }> = []

    function insertPlaceholdersUpto(index: number, appTree: Ast.Ast) {
      let canInsertPositional = true
      while (placeholdersToInsert[0] != null && placeholdersToInsert[0] < index) {
        const argIndex = placeholdersToInsert.shift()
        const argInfo = tryGetIndex(knownArguments, argIndex)
        if (argIndex != null && argInfo != null) {
          prefixArgsToDisplay.push({
            appTree,
            argument: ArgumentPlaceholder.WithRetrievedConfig(
              callId,
              argIndex,
              argInfo,
              ApplicationKind.Prefix,
              !canInsertPositional,
              widgetCfg,
            ),
          })

          canInsertPositional = false
        }
      }
    }

    for (const realArg of interpreted.args) {
      if (realArg.argName == null) {
        const argIndex = argumentsLeftToMatch.shift()
        if (argIndex != null) insertPlaceholdersUpto(argIndex, realArg.appTree.function)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: ArgumentAst.WithRetrievedConfig(
            realArg.argument,
            argIndex,
            tryGetIndex(knownArguments, argIndex),
            kind,
            widgetCfg,
          ),
        })
      } else {
        // When name is present, we need to find the argument with the same name in the list of
        // known arguments.
        const name = realArg.argName
        const foundIdx = argumentsLeftToMatch.findIndex((i) => knownArguments?.[i]?.name === name)
        const argIndex = foundIdx === -1 ? undefined : argumentsLeftToMatch.splice(foundIdx, 1)[0]

        if (argIndex != null && foundIdx === 0)
          insertPlaceholdersUpto(argIndex, realArg.appTree.function)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: ArgumentAst.WithRetrievedConfig(
            realArg.argument,
            argIndex,
            tryGetIndex(knownArguments, argIndex) ?? unknownArgInfoNamed(name),
            kind,
            widgetCfg,
          ),
        })
      }
    }

    const outerApp = interpreted.args[interpreted.args.length - 1]?.appTree ?? interpreted.func
    insertPlaceholdersUpto(Infinity, outerApp)

    return prefixArgsToDisplay.reduce(
      (target: ArgumentApplication | Ast.Ast, toDisplay) =>
        new ArgumentApplication(toDisplay.appTree, target, undefined, toDisplay.argument),
      interpreted.func,
    )
  }

  static FromInterpretedWithInfo(
    interpreted: InterpretedCall,
    callInfo: CallInfo = {},
    stripSelfArgument: boolean = false,
  ): ArgumentApplication | Ast.Ast {
    if (interpreted.kind === 'infix') {
      return ArgumentApplication.FromInterpretedInfix(interpreted, callInfo)
    } else {
      return ArgumentApplication.FromInterpretedPrefix(interpreted, callInfo, stripSelfArgument)
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
          ? this.appTree.exprId
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
  if (app instanceof Ast.PropertyAccess) return app.lhs ?? undefined
}

function isAccessOperator(opr: Ast.Token | undefined): boolean {
  return opr != null && opr.code() === '.'
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
