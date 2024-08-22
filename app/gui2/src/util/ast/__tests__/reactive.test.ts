import { Ast } from '@/util/ast'
import { reactiveModule } from '@/util/ast/reactive'
import { expect, test } from 'vitest'
import { nextTick, watchEffect } from 'vue'
import * as Y from 'yjs'

test('Module reactivity: applyEdit', async () => {
  const beforeEdit = Ast.parse('func arg1 arg2')
  beforeEdit.module.replaceRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()!.code()).toBe(beforeEdit.code())

  const app2 = module.root() as unknown as Ast.App
  let app2Code: string | undefined = undefined
  watchEffect(() => (app2Code = app2.argument.code()))
  expect(app2Code).toBe('arg2')

  const edit = beforeEdit.module.edit()
  const editApp2 = edit.getVersion(beforeEdit) as any as Ast.MutableApp
  editApp2.setArgument(Ast.Ident.tryParse('newArg', edit)!)
  const codeAfterEdit = 'func arg1 newArg'
  expect(edit.root()!.code()).toBe(codeAfterEdit)

  module.applyEdit(edit)
  expect(app2Code).toBe('arg2')
  await nextTick()
  expect(app2Code).toBe('newArg')
})

test('Module reactivity: Direct Edit', async () => {
  const beforeEdit = Ast.parse('func arg1 arg2')
  beforeEdit.module.replaceRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()!.code()).toBe(beforeEdit.code())

  const app2 = module.root() as unknown as Ast.MutableApp
  let app2Code: string | undefined = undefined
  watchEffect(() => (app2Code = app2.argument.code()))
  expect(app2Code).toBe('arg2')

  app2.setArgument(Ast.Ident.tryParse('newArg', module)!)
  const codeAfterEdit = 'func arg1 newArg'
  expect(module.root()!.code()).toBe(codeAfterEdit)

  expect(app2Code).toBe('arg2')
  await nextTick()
  expect(app2Code).toBe('newArg')
})

test('Module reactivity: Tracking access to ancestors', async () => {
  const docsBeforeEdit = 'The main method'
  const beforeEdit = Ast.parseBlock(`## ${docsBeforeEdit}\nmain =\n    23`)
  beforeEdit.module.replaceRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()!.code()).toBe(beforeEdit.code())

  const block = module.root() as any as Ast.BodyBlock
  const expression = ([...block.statements()][0] as Ast.Documented).expression as Ast.Function
  expect(expression.name.code()).toBe('main')
  let mainDocs: string | undefined = undefined
  watchEffect(() => (mainDocs = expression.documentingAncestor()?.documentation()))
  expect(mainDocs).toBe(docsBeforeEdit)

  const edit = beforeEdit.module.edit()
  const editBlock = edit.getVersion(beforeEdit) as any as Ast.MutableBodyBlock
  const editDoc = [...editBlock.statements()][0] as Ast.MutableDocumented
  const docsAfterEdit = 'The main method, now with more documentation'
  editDoc.setDocumentationText(docsAfterEdit)

  module.applyEdit(edit)
  expect(mainDocs).toBe(docsBeforeEdit)
  await nextTick()
  expect(mainDocs).toBe(docsAfterEdit)
})
