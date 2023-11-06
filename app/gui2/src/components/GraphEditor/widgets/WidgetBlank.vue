<script setup lang="ts">
import { Score, defineWidget } from '@/providers/widgetRegistry'
import { Ast, type AstExtended } from '@/util/ast'

const _props = defineProps<{ ast: AstExtended }>()
</script>

<script lang="ts">
export const widgetConfig = defineWidget({
  beforeOverride: false,
  priority: 10,
  match: (info) => (info.ast.isToken(Ast.Token.Type.Wildcard) ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span ref="rootNode" class="WidgetBlank">_</span>
</template>

<style scoped>
.WidgetBlank {
  color: transparent;
  display: inline-block;
  position: relative;
  width: 20px;
  &::before {
    content: '';
    display: block;
    position: absolute;
    width: 20px;
    height: 4px;
    border-radius: 2px;
    bottom: 0;
    background-color: var(--node-color-port);
    transition: background-color 0.2s ease;
  }
}
</style>
