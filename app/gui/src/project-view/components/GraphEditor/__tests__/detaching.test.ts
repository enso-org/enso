import { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import * as iter from 'enso-common/src/utilities/data/iter'
import { expect, test } from 'vitest'
import { watchEffect } from 'vue'
import { analyzeDetaching } from '../detaching'

function fixture(code: string) {
  const graphDb = GraphDb.Mock()
  const { root, getSpan } = Ast.parseUpdatingIdMap(code)
  const func = iter.first(root.statements())
  assert(func instanceof Ast.MutableFunctionDef)
  graphDb.updateExternalIds(root)
  graphDb.updateNodes(func, { watchEffect })
  graphDb.updateBindings(func, { text: code, getSpan })
  return { graphDb, func }
}

interface TestCase {
  description: string
  initialNodes: string[]
  selectedNodesRange: { start: number; end: number }
  changedNodes: [number, string][]
}

const cases: TestCase[] = [
  {
    description: 'Single node',
    initialNodes: ['a = data', 'b = a.operation', 'c = 3 + b'],
    selectedNodesRange: { start: 1, end: 2 },
    changedNodes: [[2, 'c = 3 + a']],
  },
  {
    description: 'Several input connections node',
    initialNodes: ['a = data', 'b = data2', 'c = a.operation b', 'd = 3 + c'],
    selectedNodesRange: { start: 2, end: 3 },
    changedNodes: [[3, 'd = 3 + a']],
  },
  {
    description: 'Multiple nodes',
    initialNodes: [
      'a = data',
      'b = data2',
      'c = data3',
      'd = b.operation a',
      'e = d.operation c',
      'f = 2 + e',
    ],
    selectedNodesRange: { start: 3, end: 5 },
    changedNodes: [[5, 'f = 2 + b']],
  },
  {
    description: 'Multiple flows',
    initialNodes: [
      'a = data',
      'b = data2',
      'c = data3',
      'd = b.operation a',
      'e = d.operation',
      'f = c.operation b',
      'g = 2 + e + f',
      'h = 3 + f',
    ],
    selectedNodesRange: { start: 3, end: 6 },
    changedNodes: [
      [6, 'g = 2 + b + c'],
      [7, 'h = 3 + c'],
    ],
  },
  {
    description: 'Detaching unavailable',
    initialNodes: [
      'a = data',
      'b = data2',
      'c = data3',
      'd = b.operation a',
      'e = Main.collapsed',
      'f = 2 + e + f',
      'g = 3 + f',
    ],
    selectedNodesRange: { start: 3, end: 5 },
    changedNodes: [],
  },
]

test.each(cases)(
  'Detaching nodes from graph: $description',
  ({ initialNodes, selectedNodesRange, changedNodes }) => {
    const code = `main =\n    ${initialNodes.join('\n    ')}`
    const { graphDb, func } = fixture(code)
    const nodeIds = [...graphDb.nodeIdToNode.keys()]
    const selected = new Set(nodeIds.slice(selectedNodesRange.start, selectedNodesRange.end))
    for (const { port, ident } of analyzeDetaching(selected, graphDb)) {
      func.module.replace(port, Ast.Ident.new(func.module, ident))
    }
    const changedNodesMap = new Map(changedNodes)
    ;[...graphDb.nodeIdToNode.entries()].forEach(([id, node], index) => {
      if (changedNodesMap.has(index)) {
        expect(node.outerAst.code()).toBe(changedNodesMap.get(index))
      } else if (!selected.has(id)) {
        expect(node.outerAst.code()).toBe(initialNodes[index])
      }
    })
  },
)
