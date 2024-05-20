import { insertNodeStatements } from '@/composables/nodeCreation'
import { Ast } from '@/util/ast'
import { initializeFFI } from 'shared/ast/ffi'
import { expect, test } from 'vitest'

await initializeFFI()

test.each([
  ['node1 = 123', '*'],
  ['node1 = 123', '*', 'node1'],
  ['node1 = 123', '', '*', 'node1'],
  ['*', 'node1'],
  ['', '*', 'node1'],
  ['*', '## Return value', 'node1'],
  ['*', '## Return value', '', 'node1'],
])('New node location in block', (...linesWithInsertionPoint: string[]) => {
  const inputLines = linesWithInsertionPoint.filter((line) => line !== '*')
  const bodyBlock = Ast.parseBlock(inputLines.join('\n'))
  insertNodeStatements(bodyBlock, [Ast.parse('newNodePositionMarker')])
  const lines = bodyBlock
    .code()
    .split('\n')
    .map((line) => (line === 'newNodePositionMarker' ? '*' : line))
  expect(lines).toEqual(linesWithInsertionPoint)
})
