import type { ForcePort } from '@/providers/portInfo'
import { AnyWidget } from '@/providers/widgetRegistry'
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

export class Argument {
  constructor(
    public kind: ApplicationKind,
    public ast: Ast.Ast | undefined,
    public index?: number | undefined,
    public argInfo?: SuggestionEntryArgument | undefined,
    public dynamicConfig?: WidgetConfiguration | undefined,
  ) {}

  static WithRetrievedConfig(
    kind: ApplicationKind,
    ast: Ast.Ast | undefined,
    index: number | undefined,
    info: SuggestionEntryArgument | undefined,
    functionCallConfig: widgetCfg.FunctionCall | undefined,
  ) {
    const cfg =
      info != null ? functionCallConfig?.parameters.get(info.name) ?? undefined : undefined
    return new Argument(kind, ast, index, info, cfg)
  }

  toAnyWidget(): AnyWidget {
    return new AnyWidget(this.ast, this.dynamicConfig, this.argInfo)
  }

  isPlaceholder() {
    return this.ast == null
  }
}

type InterpretedCall = AnalyzedInfix | AnalyzedPrefix

interface AnalyzedInfix {
  kind: 'infix'
  appTree: Ast.OprApp
  operator: Ast.Token | undefined
  lhs: Ast.Ast | undefined
  rhs: Ast.Ast | undefined
}

interface AnalyzedPrefix {
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
    public appTree: Ast.Ast | undefined,
    public target: ArgumentApplication | Argument | Ast.Ast,
    public infixOperator: Ast.Token | undefined,
    public argument: Argument,
  ) {}

  private static FromInterpretedInfix(interpreted: AnalyzedInfix, callInfo: CallInfo) {
    // Access infixes are not real infix calls.
    if (isAccessOperator(interpreted.operator)) return interpreted.appTree
    const { suggestion, widgetCfg } = callInfo

    const kind = ApplicationKind.Infix
    const argFor = (key: 'lhs' | 'rhs', index: number) => {
      const tree = interpreted[key]
      const info = tryGetIndex(suggestion?.arguments, index) ?? unknownArgInfoNamed(key)
      return tree != null
        ? Argument.WithRetrievedConfig(kind, tree, index, info, widgetCfg)
        : Argument.WithRetrievedConfig(kind, undefined, index, info, widgetCfg)
    }
    return new ArgumentApplication(
      interpreted.appTree,
      argFor('lhs', 0),
      interpreted.operator,
      argFor('rhs', 1),
    )
  }

  private static FromInterpretedPrefix(
    interpreted: AnalyzedPrefix,
    callInfo: CallInfo,
    stripSelfArgument: boolean,
  ) {
    const { noArgsCall, appMethodCall, suggestion, widgetCfg } = callInfo
    const kind = ApplicationKind.Prefix

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
      argument: Argument
    }> = []

    function insertPlaceholdersUpto(index: number, appTree: Ast.Ast) {
      while (placeholdersToInsert[0] != null && placeholdersToInsert[0] < index) {
        const argIndex = placeholdersToInsert.shift()
        const argInfo = tryGetIndex(knownArguments, argIndex)
        if (argIndex != null && argInfo != null)
          prefixArgsToDisplay.push({
            appTree,
            argument: Argument.WithRetrievedConfig(kind, undefined, argIndex, argInfo, widgetCfg),
          })
      }
    }

    for (const realArg of interpreted.args) {
      if (realArg.argName == null) {
        const argIndex = argumentsLeftToMatch.shift()
        if (argIndex != null) insertPlaceholdersUpto(argIndex, realArg.appTree)
        const info = tryGetIndex(knownArguments, argIndex)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: Argument.WithRetrievedConfig(kind, realArg.argument, argIndex, info, widgetCfg),
        })
      } else {
        // When name is present, we need to find the argument with the same name in the list of
        // known arguments.
        const name = realArg.argName
        const foundIdx = argumentsLeftToMatch.findIndex((i) => knownArguments?.[i]?.name === name)
        const argIndex = foundIdx === -1 ? undefined : argumentsLeftToMatch.splice(foundIdx, 1)[0]
        if (argIndex != null && foundIdx === 0) insertPlaceholdersUpto(argIndex, realArg.appTree)
        const info = tryGetIndex(knownArguments, argIndex) ?? unknownArgInfoNamed(name)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: Argument.WithRetrievedConfig(kind, realArg.argument, argIndex, info, widgetCfg),
        })
      }
    }

    insertPlaceholdersUpto(Infinity, interpreted.func)

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
}

const unknownArgInfoNamed = (name: string) => ({
  name,
  type: 'Any',
  isSuspended: false,
  hasDefault: false,
})

export function getAccessOprSubject(app: Ast.Ast): Ast.Ast | undefined {
  if (app instanceof Ast.PropertyAccess) return app.lhs ?? undefined
}

function isAccessOperator(opr: Ast.Token | undefined): boolean {
  return opr != null && opr.code() === '.'
}

declare const ArgumentApplicationKey: unique symbol
declare const ArgumentAstKey: unique symbol
declare const ForcePortKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [ArgumentApplicationKey]: ArgumentApplication
    [ArgumentAstKey]: Argument
    [ForcePortKey]: ForcePort
  }
}
