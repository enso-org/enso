import { Tree, type MultipleOperatorError, type Token } from '@/generated/ast'
import { assert } from '@/util/assert'
import { parseEnsoLine, readAstSpan, readTokenSpan } from '@/util/ast'
import type { Result } from '@/util/result'

/** An operand of one of the applications inside `GeneralOprApp` */
export type GeneralOperand =
  | Operand
  // A part of `GeneralOprApp`, consisting of lhs and first `statements` of applications.
  | { type: 'partOfGeneralOprApp'; oprApp: GeneralOprApp; statements: number }

/** A structure unifying API of OprApp and OperatorBlockApplication */
export class GeneralOprApp {
  lhs: Tree | null
  apps: {
    opr: Result<Token.Operator, MultipleOperatorError>
    expr: Tree | null
  }[]

  constructor(ast: Tree.OprApp | Tree.OperatorBlockApplication) {
    this.lhs = ast.lhs ?? null
    if (ast.type === Tree.Type.OprApp) {
      const rhs = ast.rhs ?? null
      this.apps = [{ opr: ast.opr, expr: rhs }]
    } else {
      function* nonEmptyLines(ast: Tree.OperatorBlockApplication) {
        for (const line of ast.expressions) {
          if (line.expression == null) continue
          yield { opr: line.expression.operator, expr: line.expression.expression }
        }
      }
      this.apps = Array.from(nonEmptyLines(ast))
    }
  }

  /** Last operator */
  lastOpr(): Result<Token.Operator, MultipleOperatorError> | null {
    return this.apps[this.apps.length - 1]?.opr ?? null
  }

  /** Returns representation of all operands interleaved with appropriate operators */
  *componentsReprs(code: string): Generator<string | null> {
    yield this.lhs != null ? readAstSpan(this.lhs, code) : null
    for (const app of this.apps) {
      yield app.opr.ok ? readTokenSpan(app.opr.value, code) : null
      yield app.expr != null ? readAstSpan(app.expr, code) : null
    }
  }

  /** Read operands of an operator chain. Operator is assumed to be left-associative.
   *
   * Works like `operandsOfLeftAssocOprChain` defined in this module, see its docs for details.
   */
  *operandsOfLeftAssocOprChain(
    code: string,
    expectedOpr?: string,
  ): Generator<GeneralOperand | null> {
    // If this represents an OperatorBlockApplication, there may be many different operators. Our chain
    // ends with the first not matching starting from the end.
    let matchingOprs
    for (matchingOprs = 0; matchingOprs < this.apps.length; matchingOprs++) {
      const app = this.apps[this.apps.length - matchingOprs - 1]!
      if (!app.opr.ok) break
      const oprCode = readTokenSpan(app.opr.value, code)
      if (expectedOpr != null && oprCode != expectedOpr) break
      expectedOpr = oprCode
    }
    if (matchingOprs === this.apps.length) {
      // If all operators matched, the lhs may be a continuation of this chain.
      if (this.lhs != null) yield* operandsOfLeftAssocOprChain(this.lhs, code, expectedOpr)
      else yield null
    } else {
      // Not all operators matched; the first operand will be a part of this GeneralOprApp.
      yield {
        type: 'partOfGeneralOprApp',
        oprApp: this,
        statements: this.apps.length - matchingOprs,
      }
    }
    for (let i = this.apps.length - matchingOprs; i < this.apps.length; ++i) {
      const app = this.apps[i]
      if (app?.expr != null) yield { type: 'ast', ast: app.expr }
      else yield null
    }
  }
}

/** An operand of some operator application chain.
 *
 * There is a special case, where operand is a part of OperatorBlockApplication which is not
 * representable by any AST structure.
 */
export type Operand =
  | { type: 'ast'; ast: Tree }
  | { type: 'partOfOprBlockApp'; ast: Tree.OperatorBlockApplication; statements: number }

/** Read operands of an operator chain. Operator is assumed to be left-associative.
 *
 * It flattens applications of same operator, e.g. for `2 + 3 + 4` will return `2`, `3`, and `4`,
 * but `2 - 3 + 4` will return `2 - 3` as first operand, and then `4`. If the ast is not
 * an operator application (of this specific operator if provided), `this` will be returned as
 *  a single operand.
 *
 * @param ast the subtree which we assume is an operator application chain.
 * @param code the code from which the entire AST was generated.
 * @param expectedOpr if specified, the chain will be of specific operator.
 */
export function* operandsOfLeftAssocOprChain(
  ast: Tree,
  code: string,
  expectedOpr?: string,
): Generator<Operand | null> {
  switch (ast.type) {
    case Tree.Type.OprApp:
    case Tree.Type.OperatorBlockApplication: {
      const oprApp = new GeneralOprApp(ast)
      for (const operand of oprApp.operandsOfLeftAssocOprChain(code, expectedOpr)) {
        if (operand == null || operand.type !== 'partOfGeneralOprApp') yield operand
        else {
          const isEntireOprApp = operand.statements === oprApp.apps.length
          if (isEntireOprApp) {
            yield { type: 'ast', ast }
          } else {
            assert(ast.type === Tree.Type.OperatorBlockApplication)
            yield { type: 'partOfOprBlockApp', ast, statements: operand.statements }
          }
        }
      }
      break
    }
    default:
      yield { type: 'ast', ast }
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  test.each([
    { code: '2 + 3', result: ['2', '+', '3'] },
    { code: '2 + 4 + 5', result: ['2 + 4', '+', '5'] },
    { code: '2\n + 3\n + 4', result: ['2', '+', '3', '+', '4'] },
    { code: '2\n - 4\n * 5', result: ['2', '-', '4', '*', '5'] },
    { code: 'foo . bar\n . baz', result: ['foo . bar', '.', 'baz'] },
    // See https://github.com/orgs/enso-org/discussions/8021
    // { code: '2 + 3\n + 4', result: ['2 + 3', '+', '4'] },
    { code: '+ 2', result: [null, '+', '2'] },
    { code: '2 +', result: ['2', '+', null] },
    { code: '.foo', result: [null, '.', 'foo'] },
    { code: 'foo.', result: ['foo', '.', null] },
  ])('Generalized infix from $code', ({ code, result }) => {
    let ast = parseEnsoLine(code)
    assert(ast != null)
    if (ast.type == Tree.Type.OprSectionBoundary) {
      ast = ast.ast
    }
    assert(ast.type === Tree.Type.OprApp || ast.type === Tree.Type.OperatorBlockApplication)
    const opr = new GeneralOprApp(ast)
    expect(Array.from(opr.componentsReprs(code))).toStrictEqual(result)
  })

  test.each([
    {
      code: '2 + 3',
      result: [
        { type: 'ast', repr: '2' },
        { type: 'ast', repr: '3' },
      ],
    },
    {
      code: '2 + 3 + 4',
      result: [
        { type: 'ast', repr: '2' },
        { type: 'ast', repr: '3' },
        { type: 'ast', repr: '4' },
      ],
    },
    {
      code: '2 * 3 + 4',
      result: [
        { type: 'ast', repr: '2 * 3' },
        { type: 'ast', repr: '4' },
      ],
    },
    {
      code: '2\n + 3\n + 4',
      result: [
        { type: 'ast', repr: '2' },
        { type: 'ast', repr: '3' },
        { type: 'ast', repr: '4' },
      ],
    },
    // See https://github.com/orgs/enso-org/discussions/8021
    // {
    //   code: '2 + 3\n + 4',
    //   result: [
    //     { type: 'ast', repr: '2' },
    //     { type: 'ast', repr: '3' },
    //     { type: 'ast', repr: '4' },
    //   ],
    // },
    // There is a bug in AST spans in some OperatorBlockApplications. Fix this test once fixed
    {
      code: '2\n * 3\n + 44',
      result: [
        { type: 'partOfOprBlockApp', repr: '2\n * 3\n + 44', statements: 1 },
        { type: 'ast', repr: '44' },
      ],
    },
    {
      code: '2\n + 3\n * 4\n + 55',
      result: [
        { type: 'partOfOprBlockApp', repr: '2\n + 3\n * 4\n + 55', statements: 2 },
        { type: 'ast', repr: '55' },
      ],
    },
    // https://github.com/orgs/enso-org/discussions/8021
    // {
    //   code: '2 * 3\n + 4',
    //   result: [
    //     { type: 'ast', repr: '2 * 3' },
    //     { type: 'ast', repr: '4' },
    //   ],
    // },
    {
      code: 'foo bar',
      result: [{ type: 'ast', repr: 'foo bar' }],
    },
    {
      code: '2 * 3',
      opr: '+',
      result: [{ type: 'ast', repr: '2 * 3' }],
    },
  ])(
    'Getting left-associative operator operands in $code',
    ({
      code,
      opr,
      result,
    }: {
      code: string
      opr?: string
      result: { type: string; repr: string; statements?: number }[]
    }) => {
      const ast = parseEnsoLine(code)
      const actual = operandsOfLeftAssocOprChain(ast, code, opr)
      const actualWithExpected = Array.from(actual, (operand, i) => {
        return { actual: operand, expected: result[i] }
      })
      for (const { actual, expected } of actualWithExpected) {
        if (expected === null) {
          expect(actual).toBeNull()
        } else {
          expect(actual?.type).toStrictEqual(expected?.type)
          if (actual?.type === 'ast') {
            expect(readAstSpan(actual.ast, code)).toStrictEqual(expected?.repr)
          } else {
            assert(actual?.type == 'partOfOprBlockApp')
            expect(readAstSpan(actual.ast, code)).toStrictEqual(expected?.repr)
            expect(actual.statements).toStrictEqual(expected?.statements)
          }
        }
      }
    },
  )
}
