<script setup lang="ts">
import { Score, defineWidget } from '@/providers/widgetRegistry'
import { Ast, type AstExtended } from '@/util/ast'
import { computed, ref } from 'vue'

const props = defineProps<{ ast: AstExtended<Ast.Token> }>()

const rootNode = ref<HTMLElement>()

const spanClass = computed(() => Ast.Token.typeNames[props.ast.inner.type])
</script>

<script lang="ts">
export const widgetConfig = defineWidget({
  beforeOverride: false,
  priority: 0,
  match: (info) => (info.ast.isToken() ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span ref="rootNode" :class="['Token', spanClass]">{{ props.ast.repr() }}</span>
</template>

<style scoped>
.Token {
  color: white;
  white-space: pre;
  align-items: center;
  color: rgb(255 255 255 / 0.33);

  &.Ident,
  &.TextSection,
  &.Digits {
    color: white;
  }

  &.TextSection,
  &.Digits {
    font-weight: bold;
  }
}
</style>
