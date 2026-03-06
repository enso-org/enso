import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { computed } from 'vue'
import type { Identifier, MutableModule } from 'ydoc-shared/ast'
import { assertNever } from 'ydoc-shared/util/assert'

const missingArgPattern = computed(() => Pattern.parseExpression('Missing_Argument.throw __'))

/** Check if given AST node represents a missing argument default value expression. */
export function exprIsMissingArgument(ast: Ast.Ast): ast is Ast.App {
  return ast instanceof Ast.App && missingArgPattern.value.test(ast)
}

/** All distinct kinds of default value assignments that we explicitly recognize. */
export type ArgumentDefaultKind = 'optional' | 'required' | 'explicit'

/** Given an argument definition AST, detect the kind of its declared default value. */
export function getArgumentDefaultKind(
  definition: Ast.ArgumentDefinition<Ast.ConcreteRefs>,
): ArgumentDefaultKind {
  if (!definition.defaultValue) return 'optional'
  if (exprIsMissingArgument(Ast.unwrapGroups(definition.defaultValue.expression.node)))
    return 'required'
  return 'explicit'
}

/** Create an initial expression AST for default value of given kind, if applicable. */
export function createDefaultExpressionOfKind(
  kind: ArgumentDefaultKind,
  argumentName: string,
): Ast.Owned<Ast.MutableExpression> | undefined {
  switch (kind) {
    case 'explicit':
      return Ast.Invalid.empty()
    case 'optional':
      return undefined
    case 'required':
      return createMissingArgumentThrow(argumentName)
    default:
      assertNever(kind)
  }
}

/** Create an AST for default value of a missing argument. */
function createMissingArgumentThrow(argumentName: string): Ast.Owned<Ast.MutableExpression> {
  const module = Ast.MutableModule.Transient()
  return missingArgPattern.value.instantiate(module, [Ast.TextLiteral.new(argumentName, module)])
}

/** Apply new argument name to the argument's default value expression, if necessary. */
export function renameArgumentInDefaultValue(
  def: Ast.ArgumentDefinition<Ast.ConcreteRefs>,
  edit: Ast.MutableModule,
  newArgumentName: string,
) {
  if (!def.defaultValue) return
  const expr = Ast.unwrapGroups(def.defaultValue.expression.node)
  if (exprIsMissingArgument(expr)) {
    const argument = Ast.unwrapGroups(expr.argument)
    if (argument instanceof Ast.TextLiteral && argument.rawTextContent != newArgumentName) {
      edit.replace(argument.id, Ast.TextLiteral.new(newArgumentName, edit))
    }
  }
}

/** Replace all variable position tokens with given name with a new token. */
export function replaceVariableUsages(
  edit: MutableModule,
  ast: Ast.Ast,
  oldNameString: string,
  newName: Ast.Owned<Ast.MutableExpression>,
  shouldRenameProps?: (access: Ast.Expression) => boolean,
) {
  const newNameString = newName.code()
  if (newNameString == oldNameString) return

  function matchIdent(node: Ast.Ast): node is Ast.Ident {
    return node instanceof Ast.Ident && node.token.code() === oldNameString
  }
  function matchIdentToken(node: Ast.Token) {
    return node.code() === oldNameString
  }

  Ast.visitRecursive(ast, (child) => {
    if (matchIdent(child)) edit.replaceValue(child.id, newName)
    else if (child instanceof Ast.PropertyAccess) {
      // Check if this property name qualifies for replacement.
      if (
        shouldRenameProps &&
        child.lhs &&
        matchIdentToken(child.rhs) &&
        shouldRenameProps(child.lhs)
      ) {
        edit.getVersion(child).setRhs(newName.code() as Ast.Identifier)
      }
      // Attempt replacing tokens on the very left side of property accesses
      return [child.lhs]
    } else if (child instanceof Ast.App) {
      // Do not replace named argument names
      return [child.function, child.argument]
    }
  })
}

/**
 * Generate an unique variable name that will not introduce collisions withing given scope.
 *
 * TODO: This should use proper scope analysis. For now, let's just look for all declared bindings within passed subtree.
 */
export function generateUniqueName(
  baseName: Identifier | ((index: number) => Identifier),
  scope: Ast.Ast | undefined,
  alwaysAllow: Iterable<string> = [],
  startingIndex = 0,
): Identifier {
  const startingName = typeof baseName === 'function' ? baseName(startingIndex) : baseName
  const generateName = typeof baseName === 'function' ? baseName : (i: number) => `${baseName}_${i}`

  if (!scope) return startingName
  const existingNames = new Set()
  Ast.visitRecursive(scope, (child) => {
    if (child instanceof Ast.Ident) existingNames.add(child.token.code())
    else if (child instanceof Ast.PropertyAccess) return [child.lhs]
    else if (child instanceof Ast.App) return [child.function, child.argument]
  })

  for (const allow of alwaysAllow ?? []) existingNames.delete(allow)

  let name = startingName
  let index = startingIndex
  while (existingNames.has(name)) name = generateName(++index) as Identifier
  return name
}
