import { useGraphStore } from '@/stores/graph'
import type { ToValue } from '@/util/reactivity'
import type { Ast } from 'shared/ast'
import { computed, toValue } from 'vue'

export function useAstDocumentation(ast: ToValue<Ast | undefined>) {
  const graphStore = useGraphStore()
  return {
    documentation: computed({
      get: () => toValue(ast)?.documentingAncestor()?.documentation() ?? '',
      set: (value) => {
        const astValue = toValue(ast)
        if (!astValue) return
        if (value.trimStart() !== '') {
          graphStore.edit((edit) =>
            edit.getVersion(astValue).getOrInitDocumentation().setDocumentationText(value),
          )
        } else {
          // Remove the documentation node.
          const documented = astValue.documentingAncestor()
          if (documented && documented.expression)
            graphStore.edit((edit) =>
              edit.getVersion(documented).update((documented) => documented.expression!.take()),
            )
        }
      },
    }),
  }
}
