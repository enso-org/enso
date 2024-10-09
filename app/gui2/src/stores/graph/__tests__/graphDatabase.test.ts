import { asNodeId, GraphDb } from '@/stores/graph/graphDatabase'
import { Ast, RawAst } from '@/util/ast'
import assert from 'assert'
import { expect, test } from 'vitest'
import { watchEffect } from 'vue'
import type { AstId } from 'ydoc-shared/ast'
import { IdMap, type ExternalId, type SourceRange } from 'ydoc-shared/yjsModel'

/** TODO: Add docs */
export function parseWithSpans<T extends Record<string, SourceRange>>(code: string, spans: T) {
  const nameToEid = new Map<keyof T, ExternalId>()
  const eid = (name: keyof T) => nameToEid.get(name)!

  const idMap = IdMap.Mock()
  let nextIndex = 0
  for (const name in spans) {
    const span = spans[name]!
    const indexStr = `${nextIndex++}`
    const eid =
      idMap.getIfExist(span) ??
      (('00000000-0000-0000-0000-000000000000'.slice(0, -indexStr.length) + indexStr) as ExternalId)
    nameToEid.set(name, eid)
    idMap.insertKnownId(span, eid)
  }

  const { root: ast, toRaw, getSpan } = Ast.parseExtended(code, idMap)
  const idFromExternal = new Map<ExternalId, AstId>()
  ast.visitRecursiveAst((ast) => {
    idFromExternal.set(ast.externalId, ast.id)
  })
  const id = (name: keyof T) => idFromExternal.get(eid(name))!

  return { ast, id, eid, toRaw, getSpan }
}

test('Reading graph from definition', () => {
  const code = `function a =
    node1 = a + 4
    node2 = node1 + 4
    node3 = node2 + 1`
  const spans = {
    functionName: [0, 8] as [number, number],
    parameter: [9, 10] as [number, number],
    node1Id: [17, 22] as [number, number],
    node1Content: [25, 30] as [number, number],
    node1LParam: [25, 26] as [number, number],
    node1RParam: [29, 30] as [number, number],
    node2Id: [35, 40] as [number, number],
    node2Content: [43, 52] as [number, number],
    node2LParam: [43, 48] as [number, number],
    node2RParam: [51, 52] as [number, number],
    node3Id: [57, 62] as [number, number],
    node3Content: [65, 74] as [number, number],
  }

  const { ast, id, eid, toRaw, getSpan } = parseWithSpans(code, spans)

  const db = GraphDb.Mock()
  const expressions = Array.from(ast.statements())
  const func = expressions[0]
  assert(func instanceof Ast.Function)
  const rawFunc = toRaw.get(func.id)
  assert(rawFunc?.type === RawAst.Tree.Type.Function)
  db.updateExternalIds(ast)
  db.updateNodes(func, { watchEffect })
  db.updateBindings(func, rawFunc, code, getSpan)

  expect(Array.from(db.nodeIdToNode.keys())).toEqual([
    eid('parameter'),
    eid('node1Content'),
    eid('node2Content'),
    eid('node3Content'),
  ])
  expect(db.getExpressionNodeId(id('node1Content'))).toBe(eid('node1Content'))
  expect(db.getExpressionNodeId(id('node1LParam'))).toBe(eid('node1Content'))
  expect(db.getExpressionNodeId(id('node1RParam'))).toBe(eid('node1Content'))
  expect(db.getExpressionNodeId(id('node2Id'))).toBeUndefined()
  expect(db.getExpressionNodeId(id('node2LParam'))).toBe(eid('node2Content'))
  expect(db.getExpressionNodeId(id('node2RParam'))).toBe(eid('node2Content'))
  expect(db.getPatternExpressionNodeId(id('node1Id'))).toBe(eid('node1Content'))
  expect(db.getPatternExpressionNodeId(id('node1Content'))).toBeUndefined()
  expect(db.getPatternExpressionNodeId(id('node2Id'))).toBe(eid('node2Content'))
  expect(db.getPatternExpressionNodeId(id('node2RParam'))).toBeUndefined()
  expect(db.getIdentDefiningNode('node1')).toBe(eid('node1Content'))
  expect(db.getIdentDefiningNode('node2')).toBe(eid('node2Content'))
  expect(db.getIdentDefiningNode('function')).toBeUndefined()
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(eid('node1Content'))))).toBe(
    'node1',
  )
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(eid('node2Content'))))).toBe(
    'node2',
  )
  expect(db.getOutputPortIdentifier(db.getNodeFirstOutputPort(asNodeId(eid('node1Id'))))).toBe(
    'node1',
  )

  expect(Array.from(db.connections.allForward(), ([key]) => key)).toEqual([
    id('parameter'),
    id('node1Id'),
    id('node2Id'),
  ])
  expect(Array.from(db.connections.lookup(id('parameter')))).toEqual([id('node1LParam')])
  expect(Array.from(db.connections.lookup(id('node1Id')))).toEqual([id('node2LParam')])
  expect(db.getOutputPortIdentifier(id('parameter'))).toBe('a')
  expect(db.getOutputPortIdentifier(id('node1Id'))).toBe('node1')
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node1Content'))))).toEqual([
    eid('node2Content'),
  ])
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node2Content'))))).toEqual([
    eid('node3Content'),
  ])
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node3Content'))))).toEqual([])
})
