import { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import * as iter from 'enso-common/src/utilities/data/iter'
import { expect, test } from 'vitest'
import { watchEffect } from 'vue'
import { analyzeConnectAround } from '../detaching'

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
  funcParameters?: string[]
  initialNodes: string[]
  selectedNodesRange: { start: number; end: number }
  expectError?: boolean
  changedNodes?: [number, string][]
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
      'h = f.write',
    ],
    selectedNodesRange: { start: 3, end: 6 },
    changedNodes: [
      [6, 'g = 2 + b + c'],
      [7, 'h = c.write'],
    ],
  },
  {
    description: 'Detaching unavailable - no input',
    initialNodes: [
      'a = data',
      'b = data2',
      'c = data3',
      'd = b.operation a',
      'e = Main.collapsed',
      'f = 2 + d + e',
      'g = 3 + e',
    ],
    selectedNodesRange: { start: 3, end: 5 },
    expectError: true,
  },
  {
    description: 'Detaching unavailable - no output',
    initialNodes: [
      'a = data',
      'b = data2',
      'c = data3',
      'd = b.operation a',
      'e = d.operation c',
      'f = 2 + c',
    ],
    selectedNodesRange: { start: 3, end: 5 },
    changedNodes: [],
  },
  {
    description: 'Reconnecting Input Node',
    funcParameters: ['x'],
    initialNodes: ['a = x.operation', 'b = a.write'],
    selectedNodesRange: { start: 0, end: 1 },
    changedNodes: [[1, 'b = x.write']],
  },
]

test.each(cases)(
  'Connecting around nodes: $description',
  ({ initialNodes, funcParameters, selectedNodesRange, expectError, changedNodes }) => {
    const code = `main ${funcParameters?.join(' ') ?? ''} =\n    ${initialNodes.join('\n    ')}`
    const { graphDb, func } = fixture(code)
    const nodeIds = [...graphDb.nodeIdToNode.entries()]
      .filter(([, node]) => node.type === 'component')
      .map(([id]) => id)
    const selected = new Set(nodeIds.slice(selectedNodesRange.start, selectedNodesRange.end))
    const analyzed = analyzeConnectAround(selected, {
      db: graphDb,
      pickInCodeOrder: (set) => {
        expect([...set.values()]).toEqual([...selected.values()])
        return [...set.values()]
      },
    })
    if (expectError) {
      expect(analyzed.ok).toBeFalsy()
    } else {
      assert(analyzed.ok)
      for (const { port, ident } of analyzed.value) {
        func.module.replace(port, Ast.Ident.new(func.module, ident))
      }
      const changedNodesMap = new Map(changedNodes)
      nodeIds.forEach((id, index) => {
        const node = graphDb.nodeIdToNode.get(id)
        if (changedNodesMap.has(index)) {
          expect(node?.outerAst.code()).toBe(changedNodesMap.get(index))
        } else if (!selected.has(id)) {
          expect(node?.outerAst.code()).toBe(initialNodes[index])
        }
      })
    }
  },
)
