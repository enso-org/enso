import { asNodeId, GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import * as iter from 'enso-common/src/utilities/data/iter'
import { expect, test } from 'vitest'
import { watchEffect } from 'vue'
import type { AstId } from 'ydoc-shared/ast'
import { SourceRange } from 'ydoc-shared/util/data/text'
import { IdMap, type ExternalId } from 'ydoc-shared/yjsModel'

/** TODO: Add docs */
export function parseWithSpans<T extends Record<string, SourceRange>>(code: string, spans: T) {
  const nameToEid = new Map<keyof T, ExternalId>()
  const eid = (name: keyof T) => nameToEid.get(name)!

  const idMap = IdMap.Mock()
  let nextIndex = 0
  for (const name in spans) {
    const span = spans[name]!
    assertDefined(span)
    const indexStr = `${nextIndex++}`
    const eid =
      idMap.getIfExist(span) ??
      (('00000000-0000-0000-0000-000000000000'.slice(0, -indexStr.length) + indexStr) as ExternalId)
    nameToEid.set(name, eid)
    idMap.insertKnownId(span, eid)
  }

  const { root: ast, getSpan } = Ast.parseUpdatingIdMap(code, idMap)
  const idFromExternal = new Map<ExternalId, AstId>()
  Ast.visitRecursive(ast, (ast) => {
    idFromExternal.set(ast.externalId, ast.id)
  })
  const id = (name: keyof T) => idFromExternal.get(eid(name))!

  return { ast, id, eid, getSpan }
}

test('Reading graph from definition', () => {
  const code = `function a =
    node1 = a + 4
    node2 = node1 + 4
    node3 = node2 + 1
    node3`
  const spans = {
    functionName: SourceRange.unsafeFromBounds(0, 8),
    parameter: SourceRange.unsafeFromBounds(9, 10),
    node1Id: SourceRange.unsafeFromBounds(17, 22),
    node1Content: SourceRange.unsafeFromBounds(25, 30),
    node1LParam: SourceRange.unsafeFromBounds(25, 26),
    node1RParam: SourceRange.unsafeFromBounds(29, 30),
    node2Id: SourceRange.unsafeFromBounds(35, 40),
    node2Content: SourceRange.unsafeFromBounds(43, 52),
    node2LParam: SourceRange.unsafeFromBounds(43, 48),
    node2RParam: SourceRange.unsafeFromBounds(51, 52),
    node3Id: SourceRange.unsafeFromBounds(57, 62),
    node3Content: SourceRange.unsafeFromBounds(65, 74),
    output: SourceRange.unsafeFromBounds(79, 84),
  } satisfies Record<string, SourceRange>

  const { ast, id, eid, getSpan } = parseWithSpans(code, spans)

  const db = GraphDb.Mock()
  const func = iter.first(ast.statements())
  assert(func instanceof Ast.FunctionDef)
  db.updateExternalIds(ast)
  db.updateNodes(func, { watchEffect })
  db.updateBindings(func, { text: code, getSpan })

  expect(Array.from(db.nodeIdToNode.keys())).toEqual([
    eid('parameter'),
    eid('node1Content'),
    eid('node2Content'),
    eid('node3Content'),
    eid('output'),
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
    id('node3Id'),
  ])
  expect(Array.from(db.connections.lookup(id('parameter')))).toEqual([id('node1LParam')])
  expect(Array.from(db.connections.lookup(id('node1Id')))).toEqual([id('node2LParam')])
  expect(Array.from(db.connections.lookup(id('node3Id')))).toEqual([id('output')])
  expect(db.getOutputPortIdentifier(id('parameter'))).toBe('a')
  expect(db.getOutputPortIdentifier(id('node1Id'))).toBe('node1')
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node1Content'))))).toEqual([
    eid('node2Content'),
  ])
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node2Content'))))).toEqual([
    eid('node3Content'),
  ])
  expect(Array.from(db.nodeDependents.lookup(asNodeId(eid('node3Content'))))).toEqual([
    eid('output'),
  ])
})
