import { Ast } from '@/util/ast'
import { MutableModule, newExternalId, ReactiveModule } from '@/util/ast/abstract'
import { initializeFFI } from 'shared/ast/ffi'
import { expect, test } from 'vitest'
import * as Y from 'yjs'

await initializeFFI()

test('Reactive module observes tree changes', () => {
  const syncModule = MutableModule.Transient()
  const reactiveModule = new ReactiveModule(syncModule)
  const { root } = Ast.parseExtended('main =\n    23')
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(root.module.ydoc))
  expect(reactiveModule.root()?.code()).toBe('main =\n    23')
  expect(reactiveModule.root()?.externalId).toBe(root.externalId)

  const edit = syncModule.edit()
  const { root: root2 } = Ast.parseExtended('main = 42', undefined, edit)
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(root2.module.ydoc))
  expect(reactiveModule.root()?.code()).toBe('main = 42')
  expect(reactiveModule.root()?.externalId).toBe(root2.externalId)
})

test('Reactive module observes metadata changes', () => {
  const syncModule = MutableModule.Transient()
  const reactiveModule = new ReactiveModule(syncModule)
  const { root } = Ast.parseExtended('main =\n    23')
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(root.module.ydoc))
  expect(reactiveModule.root()?.code()).toBe('main =\n    23')
  expect(reactiveModule.root()?.externalId).toBe(root.externalId)

  const edit = syncModule.edit()
  const newId = newExternalId()
  edit.getVersion(root).setExternalId(newId)
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(edit.ydoc))
  expect(reactiveModule.root()?.externalId).toBe(newId)
})
