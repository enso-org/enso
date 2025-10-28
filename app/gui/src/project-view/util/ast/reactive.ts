import { markRaw, shallowReactive } from 'vue'
import { MutableModule } from 'ydoc-shared/ast'
import * as Y from 'yjs'

/**
 * Make AST structures inside the module reactive (including the node's and widgets' metadata).
 *
 * Note that non-Ast structured fields (e.g. ArgumentDefinition) are not themselves reactive --
 * an access is tracked when obtaining the object from the Ast, not when accessing the inner
 * object's fields.
 *
 * **Important**: avoid watching AST structures synchronously (with `flush: 'sync'` option),
 * because you may be notified with partially updated AST data. There are no consistency
 * guarantees in this case and exceptions are likely to follow.
 */
export function reactiveModule(doc: Y.Doc, onCleanup: (f: () => void) => void): MutableModule {
  const module = markRaw(new MutableModule(doc))

  const handle = module.observe((update) => {
    update.nodesAdded.forEach((astId) => {
      const fields = module.get(astId).fields
      ;(fields as any)._map = shallowReactive((fields as any)._map)
      const metadata = fields.get('metadata')
      ;(metadata as any)._map = shallowReactive((metadata as any)._map)
      const widgetsMetadata = metadata.get('widget')
      ;(widgetsMetadata as any)._map = shallowReactive((widgetsMetadata as any)._map)
    })
  })
  onCleanup(() => module.unobserve(handle))
  return module
}
