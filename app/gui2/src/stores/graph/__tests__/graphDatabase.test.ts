import { Ast } from '@/util/ast'
import assert from 'assert'
import { IdMap, type ExprId } from 'shared/yjsModel'
import { expect, test } from 'vitest'
import { GraphDb } from '../graphDatabase'

test('Reading graph from definition', () => {
  const code = 'function a =\n    node1 = a + 4\n    node2 = node1 + 4'
  const id01 = '00000000-0000-0000-0000-000000000001' as ExprId // function
  const id02 = '00000000-0000-0000-0000-000000000002' as ExprId // a
  const id03 = '00000000-0000-0000-0000-000000000003' as ExprId // node1
  const id04 = '00000000-0000-0000-0000-000000000004' as ExprId // a + 4
  const id05 = '00000000-0000-0000-0000-000000000005' as ExprId // a
  const id06 = '00000000-0000-0000-0000-000000000006' as ExprId // 4
  const id07 = '00000000-0000-0000-0000-000000000007' as ExprId // node2
  const id08 = '00000000-0000-0000-0000-000000000008' as ExprId // node1 + 4
  const id09 = '00000000-0000-0000-0000-000000000009' as ExprId // node1
  const id10 = '00000000-0000-0000-0000-000000000010' as ExprId // 4

  const idMap = IdMap.Mock()
  idMap.insertKnownId([0, 8], id01)
  idMap.insertKnownId([9, 10], id02)
  idMap.insertKnownId([17, 22], id03)
  idMap.insertKnownId([25, 30], id04)
  idMap.insertKnownId([25, 26], id05)
  idMap.insertKnownId([29, 30], id06)
  idMap.insertKnownId([35, 40], id07)
  idMap.insertKnownId([43, 52], id08)
  idMap.insertKnownId([43, 48], id09)
  idMap.insertKnownId([51, 52], id10)

  const db = GraphDb.Mock()
  const ast = Ast.parseTransitional(code, idMap)
  assert(ast instanceof Ast.BodyBlock)
  const expressions = Array.from(ast.expressions())
  const func = expressions[0]
  assert(func instanceof Ast.Function)
  db.readFunctionAst(func, (_) => ({ x: 0.0, y: 0.0, vis: null }))

  expect(Array.from(db.nodeIdToNode.keys())).toEqual([id04, id08])
  expect(db.getExpressionNodeId(id04)).toBe(id04)
  expect(db.getExpressionNodeId(id05)).toBe(id04)
  expect(db.getExpressionNodeId(id06)).toBe(id04)
  expect(db.getExpressionNodeId(id07)).toBeUndefined()
  expect(db.getExpressionNodeId(id09)).toBe(id08)
  expect(db.getExpressionNodeId(id10)).toBe(id08)
  expect(db.getPatternExpressionNodeId(id03)).toBe(id04)
  expect(db.getPatternExpressionNodeId(id04)).toBeUndefined()
  expect(db.getPatternExpressionNodeId(id07)).toBe(id08)
  expect(db.getPatternExpressionNodeId(id10)).toBeUndefined()
  expect(db.getIdentDefiningNode('node1')).toBe(id04)
  expect(db.getIdentDefiningNode('node2')).toBe(id08)
  expect(db.getIdentDefiningNode('function')).toBeUndefined()
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(id04))).toBe('node1')
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(id08))).toBe('node2')
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(id03))).toBe('node1')

  // Commented the connection from input node, as we don't support them yet.
  expect(Array.from(db.connections.allForward(), ([key]) => key)).toEqual([id03])
  // expect(Array.from(db.connections.lookup(id02))).toEqual([id05])
  expect(Array.from(db.connections.lookup(id03))).toEqual([id09])
  // expect(db.getOutputPortIdentifier(id02)).toBe('a')
  expect(db.getOutputPortIdentifier(id03)).toBe('node1')
})
