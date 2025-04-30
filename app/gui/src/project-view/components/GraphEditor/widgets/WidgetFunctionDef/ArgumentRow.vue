<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { ArgumentDefinition, ConcreteRefs } from 'ydoc-shared/ast'
import { isSome, mapOrUndefined } from 'ydoc-shared/util/data/opt'
import { EnsoExpression } from '../WidgetEnsoExpression.vue'

const { definition } = defineProps<{
  definition: ArgumentDefinition<ConcreteRefs>
}>()
const emit = defineEmits<{
  rename: [value: Ast.Owned<Ast.MutableExpression>]
}>()

type WidgetProps = ComponentProps<typeof NodeWidget>

function defaultWidget(ast: Ast.Token | Ast.Ast): WidgetProps {
  return { input: WidgetInput.FromAst(ast) }
}

function patternWidget(pattern: Ast.Expression): WidgetProps {
  return {
    input: {
      portId: pattern.id,
      value: pattern,
      [EnsoExpression]: {
        validateInput: (ast: Ast.Expression) => ast instanceof Ast.Ident,
      },
    },
    onUpdate(update: WidgetUpdate) {
      if (
        !update.edit &&
        update.portUpdate != null &&
        'value' in update.portUpdate &&
        update.portUpdate.value instanceof Ast.Ast
      ) {
        emit('rename', update.portUpdate.value)
        return true
      }
      return false
    },
  }
}

function mkWidget<T extends Ast.Ast | Ast.Token>(
  child: () => Ast.NodeChild<T> | undefined,
  toProps: (ast: T) => WidgetProps = defaultWidget,
) {
  return computed(() => mapOrUndefined(child()?.node, toProps))
}

const allWidgetsComputed = [
  mkWidget(() => definition.open),
  mkWidget(() => definition.open2),
  mkWidget(() => definition.suspension),
  mkWidget(() => definition.pattern, patternWidget),
  mkWidget(() => definition.type?.operator),
  mkWidget(() => definition.type?.type),
  mkWidget(() => definition.close2),
  mkWidget(() => definition.defaultValue?.equals),
  mkWidget(() => definition.defaultValue?.expression),
  mkWidget(() => definition.close),
]

const allWidgets = computed(() =>
  allWidgetsComputed
    .map((c) => c.value)
    .flatMap((v, key) => (isSome(v) ? ([[key, v]] as const) : [])),
)
</script>

<template>
  <div class="ArgumentRow">
    <NodeWidget v-for="[key, props] of allWidgets" :key="key" v-bind="props" />
  </div>
</template>

<style scoped>
.ArgumentRow {
  display: flex;
  flex-direction: row;
  place-items: center;
  overflow-x: clip;
}
</style>
