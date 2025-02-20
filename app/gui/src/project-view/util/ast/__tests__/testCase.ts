import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'

export type StringsWithTypeValues = Record<string, any>
export type WithValuesInstantiated<Spec extends StringsWithTypeValues> = {
  [Name in keyof Spec]: InstanceType<Spec[Name]>
}
export type TestCase<T extends StringsWithTypeValues> = {
  statements: WithValuesInstantiated<T>
  module: Ast.Module
}

/** TODO: Add docs */
export function testCase<T extends StringsWithTypeValues>(spec: T): TestCase<T> {
  let code = ''
  for (const lineCode of Object.keys(spec)) {
    code += lineCode
    code += '\n'
  }

  const statementIndex = new Map<string, Ast.Ast>()
  const parsed = Ast.parseBlock(code)
  parsed.module.setRoot(parsed)
  const statements = new Array<Ast.Ast>()
  parsed.visitRecursive((ast) => {
    if (ast instanceof Ast.BodyBlock) statements.push(...ast.statements())
  })
  for (const statement of statements) {
    const code = statement.code()
    const trimmedFirstLine = code.split('\n', 1)[0]!.trim()
    assert(
      !statementIndex.has(trimmedFirstLine),
      'Not implemented: Disambiguating duplicate statements.',
    )
    statementIndex.set(trimmedFirstLine, statement)
  }

  const result: Partial<WithValuesInstantiated<T>> = {}
  for (const [lineCode, lineType] of Object.entries(spec)) {
    const trimmed = lineCode.trim()
    const statement = statementIndex.get(trimmed)
    assertDefined(statement)
    assert(statement instanceof lineType)
    const key: keyof WithValuesInstantiated<T> = lineCode
    result[key] = statement as any
  }
  return { statements: result as any, module: parsed.module }
}

/** TODO: Add docs */
export function tryFindExpressions<T extends StringsWithTypeValues>(
  root: Ast.Ast,
  expressions: T,
): Partial<WithValuesInstantiated<T>> {
  const result: Partial<WithValuesInstantiated<T>> = {}
  const expressionsSought = new Set(Object.keys(expressions))
  root.visitRecursive((ast) => {
    const code = ast.code()
    const trimmedFirstLine = code.split('\n', 1)[0]!.trim()
    if (!expressionsSought.has(trimmedFirstLine)) return
    const key: keyof WithValuesInstantiated<T> = trimmedFirstLine
    if (!(ast instanceof expressions[trimmedFirstLine])) return
    assert(!(key in result))
    result[key] = ast as any
  })
  return result
}

/** TODO: Add docs */
export function findExpressions<T extends StringsWithTypeValues>(
  root: Ast.Ast,
  expressions: T,
): WithValuesInstantiated<T> {
  const result = tryFindExpressions(root, expressions)
  for (const key of Object.keys(expressions)) assert(key in result)
  return result as any
}
