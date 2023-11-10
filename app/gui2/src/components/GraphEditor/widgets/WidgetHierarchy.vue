<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, defineWidget, widgetAst, type WidgetProps } from '@/providers/widgetRegistry'
import { Ast, AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps<WidgetProps>()

const spanClass = computed(() => widgetAst(props.input)?.treeTypeName())
const children = computed(() => [...(widgetAst(props.input)?.children() ?? [])])

function shouldNest(child: AstExtended, index: number) {
  return widgetAst(props.input)!.isTree(Ast.Tree.Type.App) && !child.isTree(Ast.Tree.Type.App)
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 1000,
  match: (info) => (widgetAst(info.input)?.isTree() ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span :class="['Tree', spanClass]"
    ><NodeWidget
      v-for="(child, index) in children"
      :key="child.astId"
      :input="child"
      :nest="shouldNest(child, index)"
    />
  </span>
</template>

<style scoped>
.Tree {
  white-space: pre;
  align-items: center;
  transition: background 0.2s ease;
  min-height: 24px;
  display: inline-block;

  &.Literal {
    font-weight: bold;
  }

  &.port {
    background-color: var(--node-color-port);
    border-radius: var(--node-border-radius);
    margin: -2px -4px;
    padding: 2px 4px;
  }
}
</style>
