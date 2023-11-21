<script setup lang="ts">
import {
  Score,
  defineWidget,
  widgetAst,
  widgetToken,
  type WidgetProps,
} from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed, ref } from 'vue'

const props = defineProps<WidgetProps>()

const rootNode = ref<HTMLElement>()

const spanClass = computed(() => widgetToken(props.input)?.typeName())
const repr = computed(() => widgetToken(props.input)?.code())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 1000,
  match: (info) => (info.input instanceof Ast.Tok ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span ref="rootNode" :class="['Token', spanClass]">{{ repr }}</span>
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
