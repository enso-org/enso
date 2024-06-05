<script setup lang="ts">
import MarkdownEditor from '@/components/MarkdownEditor.vue'

const documentation = defineModel<string>({ required: true })
</script>

<template>
  <MarkdownEditor v-model="documentation" />
</template>
