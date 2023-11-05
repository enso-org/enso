<script setup lang="ts">
import { Ast, type AstExtended } from '@/util/ast'
import { computed, ref } from 'vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended<Ast.Token> }>()

const rootNode = ref<HTMLElement>()

const spanClass = computed(() => Ast.Token.typeNames[props.ast.inner.type])
const whitespace = computed(() => ' '.repeat(props.ast.inner.whitespaceLengthInCodeBuffer))
</script>

<template>
  <span
    ref="rootNode"
    :class="['Token', spanClass]"
    :data-span-start="props.ast.span()[0] - nodeSpanStart - whitespace.length"
    >{{ whitespace }}{{ props.ast.repr() }}</span
  >
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
