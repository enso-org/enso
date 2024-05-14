<script setup lang="ts">
import BindKey from '@/components/lexical/BindKey.vue'
import EditorRoot from '@/components/lexical/EditorRoot.vue'
import PlainTextSync from '@/components/lexical/PlainTextSync.vue'
import { useFocusDelayed } from '@/composables/focus'
import { syncRef } from '@vueuse/core'
import { KEY_ENTER_COMMAND } from 'lexical'
import { LexicalComposer, LexicalContentEditable, LexicalPlainTextPlugin } from 'lexical-vue'
import { ref } from 'vue'

const text = defineModel<string>({ required: true })
const focused = defineModel<boolean>('focused', { default: false })
const _props = defineProps<{
  singleLine?: boolean
}>()

const root = ref<HTMLElement>()

const config = {
  editable: true,
  namespace: 'PlainTextEditor',
  theme: {},
}

syncRef(focused, useFocusDelayed(root).focused)
</script>

<template>
  <LexicalComposer :initial-config="config" @error="console.error($event)">
    <LexicalPlainTextPlugin>
      <template #contentEditable>
        <LexicalContentEditable :spellcheck="false" class="lexicalContent" />
      </template>
    </LexicalPlainTextPlugin>
    <PlainTextSync v-model="text" />
    <EditorRoot v-model="root" />
    <BindKey v-if="singleLine" :commandKey="KEY_ENTER_COMMAND" @pressed="focused = false" />
  </LexicalComposer>
</template>

<style scoped>
.lexicalContent {
  outline-style: none;
}
</style>
