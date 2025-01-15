import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { reactiveModule } from '@/util/ast/reactive'
import * as iter from 'enso-common/src/utilities/data/iter'
import { expect, test } from 'vitest'
import { nextTick, watchEffect } from 'vue'
import * as Y from 'yjs'

function getAppAtModuleRoot(module: Ast.MutableModule) {
  const expressionStatement = iter.first(module.root()!.statements())
  assert(expressionStatement instanceof Ast.MutableExpressionStatement)
  const app2 = expressionStatement.expression
  assert(app2 instanceof Ast.MutableApp)
  return app2
}

test('Module reactivity: applyEdit', async () => {
  const beforeEdit = Ast.parseModule('func arg1 arg2')
  beforeEdit.module.setRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()?.code()).toBe(beforeEdit.code())

  const app2 = getAppAtModuleRoot(module)
  let app2Code: string | undefined = undefined
  watchEffect(() => (app2Code = app2.argument.code()))
  expect(app2Code).toBe('arg2')

  const edit = beforeEdit.module.edit()
  const editApp2 = getAppAtModuleRoot(edit)
  const newArg = Ast.Ident.tryParse('newArg', edit)
  assertDefined(newArg)
  expect(newArg).toBeDefined()
  editApp2.setArgument(newArg)
  const codeAfterEdit = 'func arg1 newArg'
  expect(edit.root()?.code()).toBe(codeAfterEdit)

  module.applyEdit(edit)
  expect(app2Code).toBe('arg2')
  await nextTick()
  expect(app2Code).toBe('newArg')
})

test('Module reactivity: Direct Edit', async () => {
  const beforeEdit = Ast.parseModule('func arg1 arg2')
  beforeEdit.module.setRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()?.code()).toBe(beforeEdit.code())

  const app2 = getAppAtModuleRoot(module)
  let app2Code: string | undefined = undefined
  watchEffect(() => (app2Code = app2.argument.code()))
  expect(app2Code).toBe('arg2')

  app2.setArgument(Ast.Ident.tryParse('newArg', module)!)
  const codeAfterEdit = 'func arg1 newArg'
  expect(module.root()?.code()).toBe(codeAfterEdit)

  expect(app2Code).toBe('arg2')
  await nextTick()
  expect(app2Code).toBe('newArg')
})

test('Module reactivity: Tracking access to ancestors', async () => {
  const beforeEdit = Ast.parseModule('main = 23\nother = f')
  beforeEdit.module.setRoot(beforeEdit)

  const module = reactiveModule(new Y.Doc(), () => {})
  module.applyEdit(beforeEdit.module)
  expect(module.root()!.code()).toBe(beforeEdit.code())

  const block = module.root()
  assertDefined(block)

  const [func, otherFunc] = block.statements()
  assert(func instanceof Ast.MutableFunctionDef)
  assert(otherFunc instanceof Ast.MutableFunctionDef)
  expect(func.name.code()).toBe('main')
  expect(otherFunc.name.code()).toBe('other')
  const expression = iter.first(func.bodyExpressions())
  assert(!!expression?.isExpression())
  expect(expression.code()).toBe('23')
  const otherExpression = iter.first(otherFunc.bodyExpressions())
  assert(!!otherExpression?.isExpression())
  expect(otherExpression.code()).toBe('f')

  let parentAccesses = 0
  watchEffect(() => {
    expect(expression.parent()).toBeDefined()
    parentAccesses += 1
  })
  expect(parentAccesses).toBe(1)

  const edit = beforeEdit.module.edit()
  const replacementValue = Ast.parseExpression('replacement', edit)
  assertDefined(replacementValue)
  const taken = edit.getVersion(expression).replaceValue(replacementValue)
  edit.getVersion(otherExpression).updateValue((oe) => Ast.App.positional(oe, taken, edit))
  module.applyEdit(edit)

  expect(module.root()?.code()).toBe('main = replacement\nother = f 23')
  expect(parentAccesses).toBe(1)
  await nextTick()
  expect(parentAccesses).toBe(2)
})
