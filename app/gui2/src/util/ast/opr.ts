import { Tree, type MultipleOperatorError, type Token } from '@/generated/ast'
import { readAstSpan, readTokenSpan } from '@/util/ast'
import type { Result } from '@/util/result'
import { assert } from '../assert'
import { parseEnso2 } from '../ffi'

export type GeneralOperand =
  | Operand
  | { type: 'partOfGeneralOprApp'; oprApp: GeneralOprApp; statements: number }

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

  lastOpr(): Result<Token.Operator, MultipleOperatorError> | null {
    return this.apps[this.apps.length - 1]?.opr ?? null
  }

  *operandsOfLeftAssocOprChain(
    code: string,
    expectedOpr?: string,
  ): Generator<GeneralOperand | null> {
    // TODO[ao] explain this code
    let matchingOprs
    for (matchingOprs = 0; matchingOprs < this.apps.length; matchingOprs++) {
      const app = this.apps[this.apps.length - matchingOprs - 1]!
      if (!app.opr.ok) break
      const oprCode = readTokenSpan(app.opr.value, code)
      if (expectedOpr != null && oprCode != expectedOpr) break
      expectedOpr = oprCode
    }
    if (matchingOprs === this.apps.length) {
      if (this.lhs != null) yield* operandsOfLeftAssocOprChain(this.lhs, code, expectedOpr)
      else yield null
    } else {
      yield {
        type: 'partOfGeneralOprApp',
        oprApp: this,
        statements: this.apps.length - matchingOprs,
      }
    }
    for (let i = this.apps.length - matchingOprs; i < this.apps.length; ++i) {
      const app = this.apps[i]!
      if (app.expr != null) yield { type: 'ast', ast: app.expr }
      else yield null
    }
  }
}

export type Operand =
  | { type: 'ast'; ast: Tree }
  | { type: 'partOfOprBlockApp'; ast: Tree.OperatorBlockApplication; statements: number }

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
    { code: '2 + 2', result: ['2', '+', '2'] },
    { code: '2 + 2 + 2', result: ['2 + 2', '+', '2'] },
  ])('Generalized infix from $code', ({ code, result }) => {
    const ast = parseEnso2(code)
    assert(ast.type === Tree.Type.BodyBlock)
    const oprAst = Array.from(ast.statements)[0]?.expression
    assert(oprAst != null)
    assert(oprAst.type === Tree.Type.OprApp || oprAst.type === Tree.Type.OperatorBlockApplication)
    console.log(oprAst.opr.value.debug())
    const opr = new GeneralOprApp(oprAst)
    expect(opr.lhs !== null ? readAstSpan(opr.lhs, code) : null).toBe(result[0])
    for (let i = 1; i < result.length; i += 2) {
      const appIndex = Math.ceil(i / 2)
      const app = opr.apps[appIndex]!
      expect(app.opr.ok ? readTokenSpan(app.opr.value, code) : null).toBe(result[i])
      expect(app.expr != null ? readAstSpan(app.expr, code) : null).toBe(result[i + 1])
    }
  })
}
