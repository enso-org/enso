import { assert } from '@/util/assert'
import { RawAst, RawAstExtended } from '@/util/ast'
import { zip } from '@/util/iterable'
import { mapIterator } from 'lib0/iterator'

/** An operand of one of the applications inside `GeneralOprApp` */
export type GeneralOperand<HasIdMap extends boolean = true> =
  | Operand<HasIdMap>
  // A part of `GeneralOprApp`, consisting of lhs and first `statements` of applications.
  | { type: 'partOfGeneralOprApp'; oprApp: GeneralOprApp<HasIdMap>; statements: number }

export type OperatorChain<HasIdMap extends boolean = true> = RawAstExtended<
  RawAst.Tree.OprApp | RawAst.Tree.OperatorBlockApplication,
  HasIdMap
>

/** A structure unifying API of OprApp and OperatorBlockApplication */
export class GeneralOprApp<HasIdMap extends boolean = true> {
  lhs: RawAstExtended<RawAst.Tree, HasIdMap> | null
  apps: {
    opr: RawAstExtended<RawAst.Token.Operator, HasIdMap> | null
    expr: RawAstExtended<RawAst.Tree, HasIdMap> | null
  }[]

  constructor(ast: OperatorChain<HasIdMap>) {
    this.lhs = ast.tryMap((t) => t.lhs) ?? null
    if (ast.isTree(RawAst.Tree.Type.OprApp)) {
      const rhs = ast.tryMap((t) => t.rhs) ?? null
      const opr = ast.tryMap((t) => (t.opr.ok ? t.opr.value : undefined)) ?? null
      this.apps = [{ opr, expr: rhs }]
    } else {
      const blockApplication = ast as RawAstExtended<RawAst.Tree.OperatorBlockApplication, HasIdMap>
      const expressions = (line: RawAst.OperatorLine): RawAst.OperatorBlockExpression[] =>
        line.expression ? [line.expression] : []
      const operators = blockApplication.tryMapIter((ast) =>
        [...ast.expressions]
          .flatMap(expressions)
          .map((expr) => (expr.operator.ok ? expr.operator.value : null))
          .values(),
      )
      const exprs = blockApplication.mapIter((ast) =>
        [...ast.expressions]
          .flatMap(expressions)
          .map((expr) => expr.expression)
          .values(),
      )
      this.apps = Array.from(
        mapIterator(zip(operators, exprs), ([opr, expr]) => ({
          opr: opr ? opr : null,
          expr: expr ? expr : null,
        })),
      )
    }
  }

  /** Last operator */
  lastOpr(): RawAstExtended<RawAst.Token.Operator, HasIdMap> | null {
    return this.apps[this.apps.length - 1]?.opr ?? null
  }

  /** Returns representation of all operands interleaved with appropriate operators */
  *componentsReprs(): Generator<string | null> {
    yield this.lhs != null ? this.lhs.repr() : null
    for (const app of this.apps) {
      yield app.opr != null ? app.opr.repr() : null
      yield app.expr != null ? app.expr.repr() : null
    }
  }

  /** Read operands of an operator chain. Operator is assumed to be left-associative.
   *
   * Works like `operandsOfLeftAssocOprChain` defined in this module, see its docs for details.
   */
  *operandsOfLeftAssocOprChain(expectedOpr?: string): Generator<GeneralOperand<HasIdMap> | null> {
    // If this represents an OperatorBlockApplication, there may be many different operators. Our chain
    // ends with the first not matching starting from the end.
    let matchingOprs
    for (matchingOprs = 0; matchingOprs < this.apps.length; matchingOprs++) {
      const app = this.apps[this.apps.length - matchingOprs - 1]!
      if (!app.opr) break
      const oprCode = app.opr.repr()
      if (expectedOpr != null && oprCode != expectedOpr) break
      expectedOpr = oprCode
    }
    if (matchingOprs === this.apps.length) {
      // If all operators matched, the lhs may be a continuation of this chain.
      if (this.lhs != null) yield* operandsOfLeftAssocOprChain(this.lhs, expectedOpr)
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
export type Operand<HasIdMap extends boolean = true> =
  | { type: 'ast'; ast: RawAstExtended<RawAst.Tree, HasIdMap> }
  | {
      type: 'partOfOprBlockApp'
      ast: RawAstExtended<RawAst.Tree.OperatorBlockApplication, HasIdMap>
      statements: number
    }

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
export function* operandsOfLeftAssocOprChain<HasIdMap extends boolean = true>(
  ast: RawAstExtended<RawAst.Tree, HasIdMap>,
  expectedOpr?: string,
): Generator<Operand<HasIdMap> | null> {
  switch (ast.inner.type) {
    case RawAst.Tree.Type.OprApp:
    case RawAst.Tree.Type.OperatorBlockApplication: {
      const oprApp = new GeneralOprApp(ast as OperatorChain<HasIdMap>)
      for (const operand of oprApp.operandsOfLeftAssocOprChain(expectedOpr)) {
        if (operand == null || operand.type !== 'partOfGeneralOprApp') yield operand
        else {
          const isEntireOprApp = operand.statements === oprApp.apps.length
          if (isEntireOprApp) {
            yield { type: 'ast', ast }
          } else {
            assert(ast.isTree(RawAst.Tree.Type.OperatorBlockApplication))
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
    let ast = RawAstExtended.parseLine(code)
    if (ast.isTree(RawAst.Tree.Type.OprSectionBoundary)) {
      ast = ast.map((boundary) => boundary.ast)
    }
    assert(
      ast.isTree(RawAst.Tree.Type.OprApp) || ast.isTree(RawAst.Tree.Type.OperatorBlockApplication),
    )
    const opr = new GeneralOprApp(ast as OperatorChain<false>)
    expect(Array.from(opr.componentsReprs())).toStrictEqual(result)
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
      const ast = RawAstExtended.parseLine(code)
      const actual = operandsOfLeftAssocOprChain(ast, opr)
      const actualWithExpected = Array.from(actual, (operand, i) => {
        return { actual: operand, expected: result[i] }
      })
      for (const { actual, expected } of actualWithExpected) {
        if (expected === null) {
          expect(actual).toBeNull()
        } else {
          expect(actual?.type).toStrictEqual(expected?.type)
          if (actual?.type === 'ast') {
            expect(actual.ast.repr()).toStrictEqual(expected?.repr)
          } else {
            assert(actual?.type == 'partOfOprBlockApp')
            expect(actual.ast.repr()).toStrictEqual(expected?.repr)
            expect(actual.statements).toStrictEqual(expected?.statements)
          }
        }
      }
    },
  )
}
