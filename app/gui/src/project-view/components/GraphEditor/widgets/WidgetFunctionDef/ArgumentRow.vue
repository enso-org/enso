<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { PortId, syntheticPortId } from '@/providers/portInfo'
import {
  rewritePortValueUpdate,
  UpdateHandler,
  WidgetInput,
  WidgetUpdate,
} from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { isSome, mapOrUndefined } from '@/util/data/opt'
import { Err, Ok } from '@/util/data/result'
import { computed } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { ArgumentDefinition, ConcreteRefs, TokenType } from 'ydoc-shared/ast'
import { EnsoExpression } from '../WidgetEnsoExpression.vue'
import { EnsoTypeExpression } from '../WidgetTypeExpression.vue'

const { definition, onUpdate, portIdBase } = defineProps<{
  definition: ArgumentDefinition<ConcreteRefs>
  onUpdate: UpdateHandler
  portIdBase: PortId
}>()
const emit = defineEmits<{
  rename: [value: Ast.Owned<Ast.MutableExpression>]
  updateType: [value: Ast.Owned<Ast.MutableExpression>]
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
      [EnsoExpression]: {},
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, pattern.id, (value) => {
        if (value instanceof Ast.Ast && value instanceof Ast.Ident) {
          emit('rename', value)
          return Ok()
        } else {
          return Err('Argument name must be a valid identifier.')
        }
      })
    },
  }
}

function typeWidget(ty: Ast.Ast | undefined): WidgetProps {
  const syntheticId = syntheticPortId(portIdBase, 'type')
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(ty, () => syntheticId),
      [EnsoTypeExpression]: {},
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, syntheticId, (rawValue) => {
        const value = typeof rawValue === 'string' ? Ast.parseExpression(rawValue) : rawValue
        if (value instanceof Ast.Ast && value.isExpression()) {
          emit('updateType', value)
          return Ok()
        } else {
          return Err('Argument type must be a valid expression.')
        }
      })
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
  mkWidget(
    () =>
      definition.type?.operator ?? {
        whitespace: undefined,
        node: Ast.Token.new(':', TokenType.TypeAnnotationOperator),
      },
  ),
  computed(() => typeWidget(definition.type?.type?.node)),
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
