<script setup lang="ts">
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import { useGraphStore } from '@/stores/graph'
import { assertDefined } from 'shared/util/assert'
import { computed } from 'vue'

const graphStore = useGraphStore()

const documentation = computed({
  get: () => graphStore.methodAst?.documentingAncestor()?.documentation() ?? '',
  set: (value) => {
    graphStore.edit((edit) => {
      assertDefined(graphStore.methodAst)
      edit.getVersion(graphStore.methodAst).getOrInitDocumentation().setDocumentationText(value)
    })
  },
})
</script>

<template>
  <DocumentationEditor v-model="documentation" />
</template>
