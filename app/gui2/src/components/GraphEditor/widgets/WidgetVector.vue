<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import ListWidget from '@/components/widgets/ListWidget.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { ForcePort } from '@/providers/portInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { SuggestionEntryArgument } from '@/stores/suggestionDatabase/entry'
import { Ast, RawAst } from '@/util/ast'
import { ArgumentAst } from '@/util/callTree'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const inputAst = computed(() =>
  props.input instanceof ArgumentAst ? props.input.ast : props.input,
)
const argInfo = computed(() => (props.input instanceof ArgumentAst ? props.input.info : undefined))

const defaultConstructor = computed(() => {
  const fallback = Ast.Wildcard.new
  const config = props.config
  if (config == null) return fallback
  const [_, widgetConfig] = config.find(([name]) => name === argInfo.value?.name) ?? []
  if (
    widgetConfig &&
    widgetConfig.kind == 'Vector_Editor' &&
    widgetConfig.item_editor.kind === 'Single_Choice'
  ) {
    return () => Ast.parse(widgetConfig.item_default)
  } else {
    return fallback
  }
})

const graph = useGraphStore()
const value = computed({
  get() {
    return Array.from(inputAst.value.children()).filter(
      (child): child is Ast.Ast => child instanceof Ast.Ast,
    )
  },
  set(value) {
    const newCode = `[${value.map((item) => item.code()).join(', ')}]`
    graph.setExpressionContent(inputAst.value.astId, newCode)
  },
})

const navigator = injectGraphNavigator(true)
</script>

<script lang="ts">
function forcePort(item: WidgetInput) {
  return item instanceof Ast.Ast ? new ForcePort(item) : item
}

export const widgetDefinition = defineWidget([Ast.Ast, ArgumentAst], {
  priority: 1000,
  score: (props) => {
    const ast = props.input instanceof ArgumentAst ? props.input.ast : props.input
    return ast.treeType === RawAst.Tree.Type.Array ? Score.Perfect : Score.Mismatch
  },
})

// === VectorItem ===

export class VectorItem {
  constructor(
    public inner: WidgetInput,
    public info: SuggestionEntryArgument | undefined,
  ) {}
}

function makeItem(item: WidgetInput, info: SuggestionEntryArgument | undefined) {
  return new VectorItem(forcePort(item), info)
}

declare const VectorItemKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [VectorItemKey]: VectorItem
  }
}
</script>

<template>
  <ListWidget
    v-model="value"
    :default="defaultConstructor"
    :getKey="(item: Ast.Ast) => item.astId"
    dragMimeType="application/x-enso-ast-node"
    :toPlainText="(item: Ast.Ast) => item.code()"
    :toDragPayload="(ast: Ast.Ast) => ast.serialize()"
    :fromDragPayload="Ast.deserialize"
    :toDragPosition="(p) => navigator?.clientToScenePos(p) ?? p"
    class="WidgetVector"
    contenteditable="false"
  >
    <template #default="{ item }">
      <NodeWidget :input="makeItem(item, argInfo)" :dynamicConfig="props.config" />
    </template>
  </ListWidget>
</template>

<style scoped>
.drag-preview {
  position: fixed;
  background-color: var(--node-color-primary);
  border-radius: var(--node-border-radius);
}
</style>
