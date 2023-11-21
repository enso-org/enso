import { Tree } from '@/generated/ast'
import type { SuggestionEntryArgument } from '@/stores/suggestionDatabase/entry'
import { tryGetIndex } from './array'
import type { AstExtended } from './ast'

/**
 * Information about an argument that doesn't have an assigned value yet, therefore are not
 * represented in the AST.
 */
export class ArgumentPlaceholder {
  constructor(
    public index: number,
    public info: SuggestionEntryArgument,
  ) {}
}

export class ArgumentAst {
  constructor(
    public ast: AstExtended<Tree>,
    public index: number | undefined,
    public info: SuggestionEntryArgument | undefined,
  ) {}
}

export class ArgumentApplication {
  private constructor(
    public appTree: AstExtended<Tree> | undefined,
    public target: ArgumentApplication | AstExtended<Tree>,
    public argument: ArgumentAst | ArgumentPlaceholder,
  ) {}

  static FromAstWithInfo(
    callRoot: AstExtended<Tree>,
    knownArguments: SuggestionEntryArgument[] | undefined,
    notAppliedArguments: number[],
  ): ArgumentApplication | AstExtended<Tree> {
    interface FoundApplication {
      appTree: AstExtended<Tree.App | Tree.NamedApp>
      argument: AstExtended<Tree>
      argName: string | undefined
    }

    // Traverse the AST and find all arguments applied in sequence to the same function.
    const foundApplications: FoundApplication[] = []
    let nextApplication = callRoot
    while (nextApplication.isTree([Tree.Type.App, Tree.Type.NamedApp])) {
      const argument = nextApplication.map((t) => t.arg)
      const argName = nextApplication.isTree(Tree.Type.NamedApp)
        ? nextApplication.map((t) => t.name).repr()
        : undefined
      foundApplications.push({
        appTree: nextApplication,
        argument,
        argName,
      })
      nextApplication = nextApplication.map((t) => t.func)
    }

    // The applications are peeled away from outer to inner, so arguments are in reverse order. We
    // need to reverse them back to match them with the order in suggestion entry.
    const foundArgs = foundApplications.reverse()

    const notAppliedSet = new Set(notAppliedArguments)
    const argumentsLeftToMatch = Array.from(knownArguments ?? [], (_, i) => i).filter(
      (_, i) => !notAppliedSet.has(i),
    )

    const argsToDisplay: Array<{
      appTree: AstExtended<Tree>
      argument: ArgumentAst | ArgumentPlaceholder
    }> = []

    const unknownNamed = (name: string) => ({
      name,
      type: 'Any',
      isSuspended: false,
      hasDefault: false,
    })

    function insertPlaceholdersUpto(index: number, appTree: AstExtended<Tree>) {
      while (notAppliedArguments[0] != null && notAppliedArguments[0] < index) {
        const argIndex = notAppliedArguments.shift()
        const argInfo = tryGetIndex(knownArguments, argIndex)
        if (argIndex != null && argInfo != null)
          argsToDisplay.push({ appTree, argument: new ArgumentPlaceholder(argIndex, argInfo) })
      }
    }

    for (const realArg of foundArgs) {
      if (realArg.argName == null) {
        const argIndex = argumentsLeftToMatch.shift()
        if (argIndex != null) insertPlaceholdersUpto(argIndex, realArg.appTree)
        argsToDisplay.push({
          appTree: realArg.appTree,
          argument: new ArgumentAst(
            realArg.argument,
            argIndex,
            // If we have more arguments applied than we know about, display that argument anyway, but
            // mark it as unknown.
            tryGetIndex(knownArguments, argIndex),
          ),
        })
      } else {
        // When name is present, we need to find the argument with the same name in the list of
        // known arguments.
        const name = realArg.argName
        const foundIdx = argumentsLeftToMatch.findIndex((i) => knownArguments?.[i]?.name === name)
        const argIndex = foundIdx === -1 ? undefined : argumentsLeftToMatch.splice(foundIdx, 1)[0]
        if (argIndex != null && foundIdx === 0) insertPlaceholdersUpto(argIndex, realArg.appTree)
        argsToDisplay.push({
          appTree: realArg.appTree,
          argument: new ArgumentAst(
            realArg.argument,
            argIndex,
            tryGetIndex(knownArguments, argIndex) ?? unknownNamed(name),
          ),
        })
      }
    }

    insertPlaceholdersUpto(Infinity, nextApplication)

    return argsToDisplay.reduce(
      (target: ArgumentApplication | AstExtended<Tree>, toDisplay) =>
        new ArgumentApplication(toDisplay.appTree, target, toDisplay.argument),
      nextApplication,
    )
  }
}

declare const ArgumentApplicationKey: unique symbol
declare const ArgumentPlaceholderKey: unique symbol
declare const ArgumentAstKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [ArgumentApplicationKey]: ArgumentApplication
    [ArgumentPlaceholderKey]: ArgumentPlaceholder
    [ArgumentAstKey]: ArgumentAst
  }
}
