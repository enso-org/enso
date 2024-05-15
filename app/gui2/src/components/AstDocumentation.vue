<script setup lang="ts">
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const editing = defineModel<boolean>('editing', { default: false })
const props = defineProps<{
  ast: Ast.Ast | undefined
  /** If provided, this property will be read to obtain the current documentation text instead of finding it in the AST.
   *   This can be used to reduce reactive dependencies. */
  documentation?: string
  /** If set, the Enter key will end editing instead of inserting a newline, unless the Shift key is held. */
  preferSingleLine?: boolean | undefined
}>()

const graphStore = useGraphStore()
const documentation = computed({
  get: () =>
    props?.documentation != null ?
      props.documentation
    : props.ast?.documentingAncestor()?.documentation() ?? '',
  set: (value) => {
    const ast = props.ast
    if (!ast) return
    if (value.trimStart() !== '') {
      graphStore.edit((edit) =>
        edit.getVersion(ast).getOrInitDocumentation().setDocumentationText(value),
      )
    } else {
      // Remove the documentation node.
      const documented = props.ast?.documentingAncestor()
      if (documented && documented.expression)
        graphStore.edit((edit) =>
          edit.getVersion(documented).update((documented) => documented.expression!.take()),
        )
    }
  },
})
</script>

<template>
  <DocumentationEditor v-model="documentation" v-model:editing="editing" :preferSingleLine />
</template>
