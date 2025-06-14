import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { computed } from 'vue'
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
