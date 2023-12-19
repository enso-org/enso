import type { ForcePort } from '@/providers/portInfo'
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
    public index: number,
    public info: SuggestionEntryArgument,
    public kind: ApplicationKind,
  ) {}
}

export class ArgumentAst {
  constructor(
    public ast: Ast.Ast,
    public index: number | undefined,
    public info: SuggestionEntryArgument | undefined,
    public kind: ApplicationKind,
  ) {}
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

export class ArgumentApplication {
  private constructor(
    public appTree: Ast.Ast | undefined,
    public target: ArgumentApplication | Ast.Ast | ArgumentPlaceholder | ArgumentAst,
    public infixOperator: Ast.Token | undefined,
    public argument: Ast.Ast | ArgumentAst | ArgumentPlaceholder,
  ) {}

  static Interpret(callRoot: Ast.Ast, allowInterpretAsInfix: boolean): InterpretedCall {
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

  static FromInterpretedWithInfo(
    interpreted: InterpretedCall,
    noArgsCall: MethodCall | undefined,
    appMethodCall: MethodCall | undefined,
    suggestion: SuggestionEntry | undefined,
    stripSelfArgument: boolean = false,
  ): ArgumentApplication | Ast.Ast {
    const knownArguments = suggestion?.arguments

    if (interpreted.kind === 'infix') {
      const isAccess = isAccessOperator(interpreted.operator)
      const argFor = (key: 'lhs' | 'rhs', index: number) => {
        const tree = interpreted[key]
        const info = tryGetIndex(knownArguments, index) ?? unknownArgInfoNamed(key)
        return tree != null
          ? isAccess
            ? tree
            : new ArgumentAst(tree, 0, info, ApplicationKind.Infix)
          : new ArgumentPlaceholder(0, info, ApplicationKind.Infix)
      }
      return new ArgumentApplication(
        interpreted.appTree,
        argFor('lhs', 0),
        interpreted.operator,
        argFor('rhs', 1),
      )
    }

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
      while (placeholdersToInsert[0] != null && placeholdersToInsert[0] < index) {
        const argIndex = placeholdersToInsert.shift()
        const argInfo = tryGetIndex(knownArguments, argIndex)
        if (argIndex != null && argInfo != null)
          prefixArgsToDisplay.push({
            appTree,
            argument: new ArgumentPlaceholder(argIndex, argInfo, ApplicationKind.Prefix),
          })
      }
    }

    for (const realArg of interpreted.args) {
      if (realArg.argName == null) {
        const argIndex = argumentsLeftToMatch.shift()
        if (argIndex != null) insertPlaceholdersUpto(argIndex, realArg.appTree)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: new ArgumentAst(
            realArg.argument,
            argIndex,
            // If we have more arguments applied than we know about, display that argument anyway, but
            // mark it as unknown.
            tryGetIndex(knownArguments, argIndex),
            ApplicationKind.Prefix,
          ),
        })
      } else {
        // When name is present, we need to find the argument with the same name in the list of
        // known arguments.
        const name = realArg.argName
        const foundIdx = argumentsLeftToMatch.findIndex((i) => knownArguments?.[i]?.name === name)
        const argIndex = foundIdx === -1 ? undefined : argumentsLeftToMatch.splice(foundIdx, 1)[0]
        if (argIndex != null && foundIdx === 0) insertPlaceholdersUpto(argIndex, realArg.appTree)
        prefixArgsToDisplay.push({
          appTree: realArg.appTree,
          argument: new ArgumentAst(
            realArg.argument,
            argIndex,
            tryGetIndex(knownArguments, argIndex) ?? unknownArgInfoNamed(name),
            ApplicationKind.Prefix,
          ),
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
}

const unknownArgInfoNamed = (name: string) => ({
  name,
  type: 'Any',
  isSuspended: false,
  hasDefault: false,
})

function getAccessOprSubject(app: Ast.Ast): Ast.Ast | undefined {
  if (app instanceof Ast.PropertyAccess) return app.lhs ?? undefined
}

function isAccessOperator(opr: Ast.Token | undefined): boolean {
  return opr != null && opr.code() === '.'
}

declare const ArgumentApplicationKey: unique symbol
declare const ArgumentPlaceholderKey: unique symbol
declare const ArgumentAstKey: unique symbol
declare const ForcePortKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [ArgumentApplicationKey]: ArgumentApplication
    [ArgumentPlaceholderKey]: ArgumentPlaceholder
    [ArgumentAstKey]: ArgumentAst
    [ForcePortKey]: ForcePort
  }
}
