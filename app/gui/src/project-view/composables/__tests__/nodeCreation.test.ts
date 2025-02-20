import { insertNodeStatements } from '@/composables/nodeCreation'
import { Ast } from '@/util/ast'
import { expect, test } from 'vitest'
import { identifier } from 'ydoc-shared/ast'

test.each([
  ['node1 = 123', '*'],
  ['node1 = 123', '*', 'node1'],
  ['node1 = 123', '', '*', 'node1'],
  ['*', 'node1'],
  ['', '*', 'node1'],
  ['*', '## Return value', 'node1'],
  ['*', '## Return value', '', 'node1'],
  ['*', '## Block ends in documentation?!'],
])('New node location in block', (...linesWithInsertionPoint: string[]) => {
  const inputLines = linesWithInsertionPoint.filter((line) => line !== '*')
  const bodyBlock = Ast.parseBlock(inputLines.join('\n'))
  insertNodeStatements(bodyBlock, [Ast.parseStatement('newNodePositionMarker')!])
  const lines = bodyBlock
    .code()
    .split('\n')
    .map((line) => (line === 'newNodePositionMarker' ? '*' : line))
  expect(lines).toEqual(linesWithInsertionPoint)
})

// This is a special case because when a block is empty, adding a line requires adding *two* linebreaks.
test('Adding node to empty block', () => {
  const module = Ast.MutableModule.Transient()
  const func = Ast.FunctionDef.new(identifier('f')!, [], Ast.BodyBlock.new([], module), {
    edit: module,
  })
  const rootBlock = Ast.BodyBlock.new([], module)
  rootBlock.push(func)
  expect(rootBlock.code().trimEnd()).toBe('f =')
  insertNodeStatements(func.bodyAsBlock(), [Ast.parseStatement('newNode')!])
  expect(
    rootBlock
      .code()
      .split('\n')
      .map((line) => line.trimEnd()),
  ).toEqual(['f =', '    newNode'])
})
