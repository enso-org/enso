import { Ast } from '@/util/ast'
import * as fs from 'fs'
import { splitFileContents } from 'shared/ensoFile'
import { expect, test } from 'vitest'

// FIXME: This test pulls parts of the server code to read a fixture file. Move necessary parts of
// file format handling to shared and create a test utility for easy *.enso file fixture loading.
import { deserializeIdMap } from '../../../../ydoc-server/serialization'

test('full file IdMap round trip', () => {
  const content = fs.readFileSync(__dirname + '/fixtures/stargazers.enso').toString()
  const { code, idMapJson, metadataJson: _ } = splitFileContents(content)
  const idMapOriginal = deserializeIdMap(idMapJson!)
  const idMap = idMapOriginal.clone()
  const ast_ = Ast.parseExtended(code, idMapOriginal.clone()).root
  const ast = Ast.parseExtended(code, idMap).root
  const ast2 = Ast.normalize(ast)
  const astTT = Ast.tokenTreeWithIds(ast)
  expect(ast2.code()).toBe(ast.code())
  expect(Ast.tokenTreeWithIds(ast2), 'Print/parse preserves IDs').toStrictEqual(astTT)
  expect(Ast.tokenTreeWithIds(ast_), 'All node IDs come from IdMap').toStrictEqual(astTT)
  expect([...idMap.entries()].sort()).toStrictEqual([...idMapOriginal.entries()].sort())
})
