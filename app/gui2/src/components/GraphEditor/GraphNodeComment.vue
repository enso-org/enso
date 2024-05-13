<script setup lang="ts">
import AstDocumentation from '@/components/AstDocumentation.vue'
import { type Node } from '@/stores/graph'

const editing = defineModel<boolean>('editing', { required: true })
const props = defineProps<{ node: Node }>()
</script>
<template>
  <div v-if="editing || props.node.documentation?.trimStart()" class="GraphNodeComment">
    <AstDocumentation
      v-model:editing="editing"
      :ast="props.node.outerExpr"
      :documentation="props.node.documentation ?? ''"
      preferSingleLine
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
