<script setup lang="ts">
import PlainTextEditor from '@/components/PlainTextEditor.vue'
import { useAstDocumentation } from '@/composables/astDocumentation'
import { useFocusDelayed } from '@/composables/focus'
import { type Node } from '@/stores/graph'
import { syncRef } from '@vueuse/core'
import { computed, ref, type ComponentInstance } from 'vue'

const editing = defineModel<boolean>('editing', { required: true })
const props = defineProps<{ node: Node }>()

const textEditor = ref<ComponentInstance<typeof PlainTextEditor>>()

const { documentation: astDocumentation } = useAstDocumentation(() => props.node.outerExpr)
const documentation = computed({
  // This returns the same value as the `astDocumentation` getter, but with fewer reactive dependencies.
  get: () => props.node.documentation ?? '',
  set: (value) => (astDocumentation.value = value),
})

syncRef(editing, useFocusDelayed(textEditor).focused)
</script>
<template>
  <div v-if="editing || props.node.documentation?.trimStart()" class="GraphNodeComment">
    <PlainTextEditor
      ref="textEditor"
      v-model="documentation"
      @keydown.enter.capture.stop="editing = false"
    />
  </div>
</template>

<style scoped>
.GraphNodeComment > :deep(*) {
  display: inline-block;
  padding: 0 8px 0 8px;
  min-width: 22px;
  border-radius: var(--radius-default);
  background-color: var(--node-color-no-type);
  color: var(--color-text-inversed);
  font-weight: 400;
}

.GraphNodeComment :deep(code) {
  color: var(--color-text-inversed);
}
</style>
