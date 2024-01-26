import { Ast } from '@/util/ast'
import { MutableModule, ReactiveModule } from '@/util/ast/abstract'
import { expect, test } from 'vitest'
import * as Y from 'yjs'

test('Reactive module observes Y.Js changes', () => {
  const syncModule = MutableModule.Transient()
  const reactiveModule = new ReactiveModule(syncModule)
  const { root } = Ast.parseExtended('main =\n    23')
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(root.module.ydoc))
  expect(reactiveModule.root()?.code()).toBe('main =\n    23')

  const edit = syncModule.edit()
  const { root: root2 } = Ast.parseExtended('main = 42', undefined, edit)
  Y.applyUpdateV2(syncModule.ydoc, Y.encodeStateAsUpdateV2(root2.module.ydoc))
  expect(reactiveModule.root()?.code()).toBe('main = 42')
})
