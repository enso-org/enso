import { Ast } from '@/util/ast'
import { expect, test } from 'vitest'
import { MutableModule } from 'ydoc-shared/ast'
import { SourceDocument } from 'ydoc-shared/ast/sourceDocument'
import { applyTextEdits } from 'ydoc-shared/util/data/text'
import * as Y from 'yjs'

test('Test SourceDocument', () => {
  const syncModule = new MutableModule(new Y.Doc())
  const sourceDoc = SourceDocument.Empty()
  syncModule.observe((update) => sourceDoc.applyUpdate(syncModule, update))
  const code = '1 + 1'
  const edit1 = syncModule.edit()
  const root = Ast.parseBlock(code, edit1)
  edit1.setRoot(root)
  syncModule.applyEdit(edit1)
  expect(sourceDoc.text).toBe(code)

  let observedText = ''
  sourceDoc.observe((textEdits) => {
    observedText = applyTextEdits(observedText, textEdits)
  })
  expect(observedText).toBe(code)

  const newCode = '2'
  const edit2 = syncModule.edit()
  edit2.getVersion(root).syncToCode(newCode)
  syncModule.applyEdit(edit2)
  expect(sourceDoc.text).toBe(newCode)
  expect(observedText).toBe(newCode)
})
