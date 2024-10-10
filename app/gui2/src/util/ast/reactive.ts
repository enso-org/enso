import { markRaw, shallowReactive } from 'vue'
import { MutableModule } from 'ydoc-shared/ast'
import * as Y from 'yjs'

/** TODO: Add docs */
export function reactiveModule(doc: Y.Doc, onCleanup: (f: () => void) => void): MutableModule {
  const module = markRaw(new MutableModule(doc))
  const handle = module.observe((update) => {
    update.nodesAdded.forEach((astId) => {
      const fields = module.get(astId).fields
      ;(fields as any)._map = shallowReactive((fields as any)._map)
    })
  })
  onCleanup(() => module.unobserve(handle))
  return module
}
