import { GraphDb } from '@/stores/graph/graphDatabase'
import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import { Ast, AstExtended } from '@/util/ast'
import { ComputedValueRegistry } from '@/util/computedValueRegistry'
import { IdMap, type NodeMetadata } from 'shared/yjsModel'
import { expect, test } from 'vitest'
import { ref } from 'vue'
import * as Y from 'yjs'

function mockGraphDbFromCode(code: string) {
  const doc = new Y.Doc()
  const yIds = doc.getMap<Uint8Array>('ids')
  const yCode = doc.getText('code')
  yCode.insert(0, code)
  const suggestionDb = new SuggestionDb()
  const groups = ref<Group[]>([])
  const valuesRegistry = ComputedValueRegistry.Mock()
  return {
    doc,
    graphDb: new GraphDb(suggestionDb, groups, valuesRegistry, () => 200),
    idMap: new IdMap(yIds, yCode),
  }
}

function expectFn(ast: AstExtended<Ast.Tree.Tree, true>) {
  let fn!: AstExtended<Ast.Tree.Function>
  ast.visitRecursive((node) => {
    if (node.isTree(Ast.Tree.Type.Function)) {
      fn = node
      return false
    }
    return true
  })
  expect(fn).toBeDefined()
  return fn
}

function* getFunctionNodeExpressions(func: Ast.Tree.Function): Generator<Ast.Tree> {
  if (func.body) {
    if (func.body.type === Ast.Tree.Type.BodyBlock) {
      for (const stmt of func.body.statements) {
        if (stmt.expression && stmt.expression.type !== Ast.Tree.Type.Function) {
          yield stmt.expression
        }
      }
    } else {
      yield func.body
    }
  }
}

function getExprId(expr: AstExtended<Ast.Tree.Tree, true>) {
  return expr.isTree(Ast.Tree.Type.Assignment) ? expr.map((t) => t.expr).astId : expr.astId
}

test.each([
  {
    code: `\
main =
  a = 1
  b = 2
  c = 3
  d = 4
  e = 5`,
  },
])('New nodes are created below all other nodes (without existing positions)', ({ code }) => {
  const { graphDb, idMap } = mockGraphDbFromCode(code)
  const fn = expectFn(AstExtended.parse(code, idMap))
  graphDb.readFunctionAst(fn, () => undefined)
  let bottom = -Infinity
  for (const expr of fn.visit(getFunctionNodeExpressions)) {
    const node = graphDb.nodes.get(getExprId(expr))
    expect(node).toBeDefined()
    expect(node?.position.y).toBeGreaterThan(bottom)
    bottom = Math.max(bottom, node?.position.y ?? -Infinity)
  }
})

test.each([
  {
    code: `\
main =
  a = 1
  b = 2
  c = 3
  d = 4
  e = 5`,
  },
])('New nodes are created below all other nodes (with existing positions)', ({ code }) => {
  const { doc, graphDb, idMap } = mockGraphDbFromCode(code)
  const yMetadata = doc.getMap<NodeMetadata>('metadata')
  const fn = expectFn(AstExtended.parse(code, idMap))
  let count = 0
  for (const expr of fn.visit(getFunctionNodeExpressions)) {
    yMetadata.set(getExprId(expr), { x: 0, y: -count * 32, vis: null })
    count += 1
    if (count > 2) break
  }
  graphDb.readFunctionAst(fn, (id) => yMetadata.get(id))
  let bottom = -Infinity
  for (const expr of fn.visit(getFunctionNodeExpressions)) {
    const node = graphDb.nodes.get(getExprId(expr))
    expect(node).toBeDefined()
    expect(node?.position.y).toBeGreaterThan(bottom)
    bottom = Math.max(bottom, node?.position.y ?? -Infinity)
  }
})
