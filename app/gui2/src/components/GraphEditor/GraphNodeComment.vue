<script setup lang="ts">
import PlainTextEditor from '@/components/PlainTextEditor.vue'
import { useAstDocumentation } from '@/composables/astDocumentation'
import { type Node } from '@/stores/graph'
import { computed } from 'vue'

const editing = defineModel<boolean>('editing', { required: true })
const props = defineProps<{ node: Node }>()

const { documentation: astDocumentation } = useAstDocumentation(() => props.node.outerExpr)
const documentation = computed({
  // This returns the same value as the `astDocumentation` getter, but with fewer reactive dependencies.
  get: () => props.node.documentation ?? '',
  set: (value) => (astDocumentation.value = value),
})
</script>
<template>
  <div v-if="editing || props.node.documentation?.trimStart()" class="GraphNodeComment">
    <PlainTextEditor v-model="documentation" v-model:focused="editing" singleLine />
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
