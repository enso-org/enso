import { type GraphStore } from '@/stores/graph'
import { useBufferedWritable, type ToValue } from '@/util/reactivity'
import type { Ast } from 'shared/ast'
import { toValue } from 'vue'

export function useAstDocumentation(graphStore: GraphStore, ast: ToValue<Ast | undefined>) {
  return {
    documentation: useBufferedWritable({
      get: () => toValue(ast)?.documentingAncestor()?.documentation() ?? '',
      set: (value) => {
        const astValue = toValue(ast)
        if (!astValue) return
        if (value.trimStart() !== '') {
          graphStore.edit(
            (edit) =>
              edit.getVersion(astValue).getOrInitDocumentation().setDocumentationText(value),
            true,
            true,
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
