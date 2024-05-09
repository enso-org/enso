<script setup lang="ts">
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<{
  ast: Ast.Ast | undefined
}>()

const graphStore = useGraphStore()

const documentation = computed({
  get: () => props.ast?.documentingAncestor()?.documentation() ?? '',
  set: (value) => {
    const ast = props.ast
    if (!ast) return
    graphStore.edit((edit) =>
      edit.getVersion(ast)?.getOrInitDocumentation().setDocumentationText(value),
    )
  },
})
</script>

<template>
  <DocumentationEditor v-model="documentation" />
</template>
