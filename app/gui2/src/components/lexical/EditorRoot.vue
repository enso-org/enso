<script setup lang="ts">
import { useLexicalComposer } from 'lexical-vue'
import { onMounted, onUnmounted } from 'vue'

const element = defineModel<HTMLElement | undefined>({ required: true })

const editor = useLexicalComposer()

onMounted(() => {
  const unregister = editor.registerRootListener((root) => (element.value = root ?? undefined))
  onUnmounted(unregister)
})
</script>

<!-- tslint:disable:vue/valid-template-root -->
<template />
