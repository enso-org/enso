import { prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import { GraphDb, type NodeId } from '@/stores/graph/graphDatabase'
import { assert } from '@/util/assert'
import { Ast, RawAst } from '@/util/ast'
import { unwrap } from '@/util/data/result'
import { tryIdentifier } from '@/util/qualifiedName'
import { expect, test } from 'vitest'
import { watchEffect } from 'vue'

function setupGraphDb(code: string, graphDb: GraphDb) {
  const { root, toRaw, getSpan } = Ast.parseExtended(code)
  const expressions = Array.from(root.statements())
  const func = expressions[0]
  assert(func instanceof Ast.Function)
  const rawFunc = toRaw.get(func.id)
  assert(rawFunc?.type === RawAst.Tree.Type.Function)
  graphDb.updateExternalIds(root)
  graphDb.updateNodes(func, { watchEffect })
  graphDb.updateBindings(func, rawFunc, code, getSpan)
}

interface TestCase {
  description: string
  initialNodes: string[]
  selectedNodesRange: { start: number; end: number }
  expected: {
    extracted: {
      inputs: string[]
      nodes: string[]
      output: string
    }
    refactored: {
      replace: string
      with: { pattern: string; arguments: string[] }
    }
  }
}

const testCases: TestCase[] = [
  {
    description: 'Basic',
    initialNodes: ['a = 1', 'b = 2', 'c = A + B', 'd = a + b', 'e = c + 7'],
    selectedNodesRange: { start: 1, end: 4 },
    expected: {
      extracted: {
        inputs: ['a'],
        nodes: ['b = 2', 'c = A + B', 'd = a + b'],
        output: 'c',
      },
      refactored: {
        replace: 'c = A + B',
        with: { pattern: 'c', arguments: ['a'] },
      },
    },
  },
  {
    description: 'Refactoring a single assignment',
    initialNodes: ['a = 1', 'b = 2', 'c = A + B', 'd = a + b', 'e = c + 7'],
    selectedNodesRange: { start: 3, end: 4 },
    expected: {
      extracted: {
        inputs: ['a', 'b'],
        nodes: ['d = a + b'],
        output: 'd',
      },
      refactored: {
        replace: 'd = a + b',
        with: { pattern: 'd', arguments: ['a', 'b'] },
      },
    },
  },
  {
    description: 'No arguments and multiple result usages',
    initialNodes: ['c = 50 + d', 'x = c + c + 10'],
    selectedNodesRange: { start: 0, end: 1 },
    expected: {
      extracted: {
        inputs: [],
        nodes: ['c = 50 + d'],
        output: 'c',
      },
      refactored: {
        replace: 'c = 50 + d',
        with: { pattern: 'c', arguments: [] },
      },
    },
  },
  {
    description: 'Regression https://github.com/enso-org/ide/issues/1234',
    initialNodes: [
      'number1 = 1',
      'number2 = 2',
      'range = number1.up_to number2',
      'vector = range.to_vector',
    ],
    selectedNodesRange: { start: 2, end: 4 },
    expected: {
      extracted: {
        inputs: ['number1', 'number2'],
        nodes: ['range = number1.up_to number2', 'vector = range.to_vector'],
        output: 'vector',
      },
      refactored: {
        replace: 'vector = range.to_vector',
        with: { pattern: 'vector', arguments: ['number1', 'number2'] },
      },
    },
  },
  {
    description: 'Handle lambda arguments https://github.com/enso-org/enso/issues/9412',
    initialNodes: [
      'number1 = 1',
      'number2 = 2',
      'lambda = number1 -> x -> number1 + number2 + x',
      'sum = lambda 1 2',
    ],
    selectedNodesRange: { start: 2, end: 4 },
    expected: {
      extracted: {
        inputs: ['number2'],
        nodes: ['lambda = number1 -> x -> number1 + number2 + x', 'sum = lambda 1 2'],
        output: 'sum',
      },
      refactored: {
        replace: 'sum = lambda 1 2',
        with: { pattern: 'sum', arguments: ['number2'] },
      },
    },
  },
  {
    description: 'Single input used twice https://github.com/enso-org/enso/issues/9412',
    initialNodes: ['number1 = 1', 'sum = number1 + number1'],
    selectedNodesRange: { start: 1, end: 2 },
    expected: {
      extracted: {
        inputs: ['number1'],
        nodes: ['sum = number1 + number1'],
        output: 'sum',
      },
      refactored: {
        replace: 'sum = number1 + number1',
        with: { pattern: 'sum', arguments: ['number1'] },
      },
    },
  },
]

test.each(testCases)('Collapsing nodes, $description', (testCase) => {
  const initialCode =
    'main = \n' + testCase.initialNodes.map((code) => ' '.repeat(4) + code).join('\n')
  const graphDb = GraphDb.Mock()
  setupGraphDb(initialCode, graphDb)
  const expectedExtracted = testCase.expected.extracted
  const expectedRefactored = testCase.expected.refactored
  const nodes = Array.from(graphDb.nodeIdToNode.entries())
  expect(nodes.length).toEqual(testCase.initialNodes.length)
  const nodeCodeToId = new Map<string, NodeId>()
  const nodePatternToId = new Map<string, NodeId>()
  for (const code of testCase.initialNodes) {
    const [pattern, expr] = code.split(/\s*=\s*/)
    const [id, _] = nodes.find(([_id, node]) => node.innerExpr.code() == expr)!
    nodeCodeToId.set(code, id)
    if (pattern != null) nodePatternToId.set(pattern, id)
  }
  assert(nodeCodeToId.size == nodePatternToId.size)
  const range = testCase.selectedNodesRange
  const selectedNodesCode = testCase.initialNodes.slice(range.start, range.end)
  const selectedNodes = new Set(selectedNodesCode.map((code) => nodeCodeToId.get(code)!))

  const { extracted, refactored } = unwrap(prepareCollapsedInfo(selectedNodes, graphDb))
  const expectedInputs = expectedExtracted.inputs.map((s) => unwrap(tryIdentifier(s)))
  const expectedOutput = unwrap(tryIdentifier(expectedExtracted.output))
  const expectedIds = expectedExtracted.nodes.map((code) => nodeCodeToId.get(code)!)
  const expectedRefactoredId = nodeCodeToId.get(expectedRefactored.replace)!

  expect(extracted.inputs).toEqual(expectedInputs)
  expect(extracted.output).toEqual({
    node: nodePatternToId.get(expectedOutput),
    identifier: expectedOutput,
  })
  expect(extracted.ids).toEqual(new Set(expectedIds))
  expect(refactored.id).toEqual(expectedRefactoredId)
  expect(refactored.pattern).toEqual(expectedRefactored.with.pattern)
  expect(refactored.arguments).toEqual(expectedRefactored.with.arguments)
})

test('Collapsing nodes in collapsed function', () => {
  const initialCode = `
collapsed1 input =
    four = 4
    sum = input + four
    sum

main =
    input = 14
    sum = Main.collapsed1 input`

  const graphDb = GraphDb.Mock()
  setupGraphDb(initialCode, graphDb)
  const nodes = Array.from(graphDb.nodeIdToNode.keys())
  const { extracted, refactored } = unwrap(
    prepareCollapsedInfo(new Set(nodes.slice(2, 3)), graphDb),
  )
  expect(extracted.ids).toEqual(new Set(nodes.slice(2, 3)))
  expect(extracted.inputs).toEqual(['input', 'four'])
  expect(extracted.output).toEqual({
    node: nodes[2],
    identifier: 'sum',
  })
  expect(refactored.id).toEqual(nodes[2])
  expect(refactored.pattern).toEqual('sum')
  expect(refactored.arguments).toEqual(['input', 'four'])
})
