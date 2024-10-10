import { type GraphStore } from '@/stores/graph'
import { type ToValue } from '@/util/reactivity'
import { computed, toValue } from 'vue'
import type { Ast } from 'ydoc-shared/ast'

/** A composable for reactively retrieving and setting documentation from given Ast node. */
export function useAstDocumentation(graphStore: GraphStore, ast: ToValue<Ast | undefined>) {
  return {
    documentation: {
      state: computed(() => toValue(ast)?.documentingAncestor()?.documentation() ?? ''),
      set: (value: string) => {
        const astValue = toValue(ast)
        if (!astValue) return
        if (value.trimStart() !== '') {
          graphStore.getMutable(astValue).getOrInitDocumentation().setDocumentationText(value)
        } else {
          // Remove the documentation node.
          const documented = astValue.documentingAncestor()
          if (documented && documented.expression)
            graphStore.edit((edit) =>
              edit.getVersion(documented).update((documented) => documented.expression!.take()),
            )
        }
      },
    },
  }
}
