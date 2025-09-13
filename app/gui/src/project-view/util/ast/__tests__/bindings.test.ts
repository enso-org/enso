import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { findModuleMethod } from '@/util/ast/abstract'
import { analyzeBindings } from '@/util/ast/bindings'
import { expect, test } from 'vitest'
import { MutableModule } from 'ydoc-shared/ast'
import { SourceDocument } from 'ydoc-shared/ast/sourceDocument'
import * as Y from 'yjs'

test('Bad foreign function', () => {
  const syncModule = new MutableModule(new Y.Doc())
  const sourceDoc = SourceDocument.Empty()
  syncModule.observe((update) => sourceDoc.applyUpdate(syncModule, update))
  const code = 'foreign python wat = @@@\n    return 1\n\nmain =\n    node1 = 23'
  const edit1 = syncModule.edit()
  const root = Ast.parseBlock(code, edit1)
  edit1.setRoot(root)
  syncModule.applyEdit(edit1)
  assert(sourceDoc.text === code)
  const syncModuleRoot = syncModule.root()
  assertDefined(syncModuleRoot)
  const mainMethod = findModuleMethod(syncModuleRoot, 'main')
  assertDefined(mainMethod)
  // In #12497, an assertion fails here.
  const bindings = analyzeBindings(mainMethod.statement, sourceDoc)
  expect(bindings.size).toBe(2)
})
