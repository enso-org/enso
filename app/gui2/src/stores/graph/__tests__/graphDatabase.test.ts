import { asNodeId, GraphDb } from '@/stores/graph/graphDatabase'
import { Ast, RawAst } from '@/util/ast'
import assert from 'assert'
import { IdMap, type ExternalId } from 'shared/yjsModel'
import { expect, test } from 'vitest'

/**
 * Create a predictable fake UUID which contains given number in decimal at the end.
 * @param x sequential value, e.g. 15
 * @returns fake uuid, e.g. 00000000-0000-0000-0000-000000000015
 */
function eid(x: number): ExternalId {
  const xStr = `${x}`
  return ('00000000-0000-0000-0000-000000000000'.slice(0, -xStr.length) + xStr) as ExternalId
}

test('Reading graph from definition', () => {
  const code = `function a =
    node1 = a + 4
    node2 = node1 + 4
    node3 = node2 + 1`

  const idMap = IdMap.Mock()
  idMap.insertKnownId([0, 8], eid(1)) // function
  idMap.insertKnownId([9, 10], eid(2)) // a
  idMap.insertKnownId([17, 22], eid(3)) // node1
  idMap.insertKnownId([25, 30], eid(4)) // a + 4
  idMap.insertKnownId([25, 26], eid(5)) // a
  idMap.insertKnownId([29, 30], eid(6)) // 4
  idMap.insertKnownId([35, 40], eid(7)) // node2
  idMap.insertKnownId([43, 52], eid(8)) // node1 + 4
  idMap.insertKnownId([43, 48], eid(9)) // node1
  idMap.insertKnownId([51, 52], eid(10)) // 4
  idMap.insertKnownId([57, 62], eid(11)) // node3
  idMap.insertKnownId([65, 74], eid(12)) // node2 + 1

  const db = GraphDb.Mock()
  const { root: ast, toRaw, getSpan } = Ast.parseExtended(code, idMap)
  const expressions = Array.from(ast.statements())
  const func = expressions[0]
  assert(func instanceof Ast.Function)
  const rawFunc = toRaw.get(func.id)
  assert(rawFunc?.type === RawAst.Tree.Type.Function)
  db.readFunctionAst(func, rawFunc, code, (_) => ({ x: 0.0, y: 0.0, vis: null }), getSpan)

  const idFromExternal = new Map()
  ast.visitRecursiveAst((ast) => idFromExternal.set(ast.externalId, ast.id))
  const id = (x: number) => idFromExternal.get(eid(x))!

  expect(Array.from(db.nodeIdToNode.keys())).toEqual([id(4), id(8), id(12)])
  expect(db.getExpressionNodeId(id(4))).toBe(id(4))
  expect(db.getExpressionNodeId(id(5))).toBe(id(4))
  expect(db.getExpressionNodeId(id(6))).toBe(id(4))
  expect(db.getExpressionNodeId(id(7))).toBeUndefined()
  expect(db.getExpressionNodeId(id(9))).toBe(id(8))
  expect(db.getExpressionNodeId(id(10))).toBe(id(8))
  expect(db.getPatternExpressionNodeId(id(3))).toBe(id(4))
  expect(db.getPatternExpressionNodeId(id(4))).toBeUndefined()
  expect(db.getPatternExpressionNodeId(id(7))).toBe(id(8))
  expect(db.getPatternExpressionNodeId(id(10))).toBeUndefined()
  expect(db.getIdentDefiningNode('node1')).toBe(id(4))
  expect(db.getIdentDefiningNode('node2')).toBe(id(8))
  expect(db.getIdentDefiningNode('function')).toBeUndefined()
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(id(4))))).toBe('node1')
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(id(8))))).toBe('node2')
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(id(3))))).toBe('node1')

  // Commented the connection from input node, as we don't support them yet.
  expect(Array.from(db.connections.allForward(), ([key]) => key)).toEqual([id(3), id(7)])
  // expect(Array.from(db.connections.lookup(id(2)))).toEqual([id(5)])
  expect(Array.from(db.connections.lookup(id(3)))).toEqual([id(9)])
  // expect(db.getOutputPortIdentifier(id(2))).toBe('a')
  expect(db.getOutputPortIdentifier(id(3))).toBe('node1')
  expect(Array.from(db.dependantNodes(asNodeId(id(4))))).toEqual([id(8), id(12)])
  expect(Array.from(db.dependantNodes(asNodeId(id(8))))).toEqual([id(12)])
  expect(Array.from(db.dependantNodes(asNodeId(id(12))))).toEqual([])
})
