<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { PortId, syntheticPortId } from '@/providers/portInfo'
import {
  rewritePortValueUpdate,
  UpdateHandler,
  WidgetInput,
  WidgetUpdate,
} from '@/providers/widgetRegistry'
import { injectProjectNames } from '@/stores/projectNames'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Ast } from '@/util/ast'
import { mapOrUndefined } from '@/util/data/opt'
import { Err, Ok } from '@/util/data/result'
import { computed } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { ArgumentDefinition, ConcreteRefs } from 'ydoc-shared/ast'
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
  updateDefault: [value: Ast.Owned<Ast.MutableExpression>]
}>()
type WidgetProps = ComponentProps<typeof NodeWidget>
const suggestionDb = useSuggestionDbStore()
const projectNames = injectProjectNames()

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

function mkWidget<T extends Ast.Ast | Ast.Token>(
  child: () => Ast.NodeChild<T> | undefined,
  toProps: (ast: T) => WidgetProps = defaultWidget,
) {
  return computed(() => mapOrUndefined(child()?.node, toProps))
}

const nodeSuspension = mkWidget(() => definition.suspension)
const nodePattern = mkWidget(() => definition.pattern, patternWidget)
const nodeType = computed((): WidgetProps => {
  const ty = definition.type?.type?.node
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
})

function resolveType(typeExpr: Ast.Ast) {
  const tyCode = typeExpr.code()
  // Hack: We have to resolve the fully qualified type name ourselves based on present imports.
  // To avoid implementing that for now, we only look up types selectable from dropdown.
  const matchingTypeEntry = suggestionDb.entries.selectableTypes.value.find(
    (ty) => ty.name === tyCode,
  )
  return matchingTypeEntry ?
      projectNames.printProjectPath(matchingTypeEntry.definitionPath)
    : undefined
}

const nodeDefault = computed((): WidgetProps => {
  const expr = definition.defaultValue?.expression?.node
  const syntheticId = syntheticPortId(portIdBase, 'default')
  const expectedType = mapOrUndefined(definition.type?.type?.node, resolveType)
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(expr, () => syntheticId),
      expectedType,
      [EnsoExpression]: {
        weakMatch: true,
      },
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, syntheticId, (rawValue) => {
        const value = typeof rawValue === 'string' ? Ast.parseExpression(rawValue) : rawValue
        if (value instanceof Ast.Ast && value.isExpression()) {
          emit('updateDefault', value)
          return Ok()
        } else {
          return Err('Argument default value must be a valid expression.')
        }
      })
    },
  }
})
</script>

<template>
  <div class="ArgumentRow">
    <NodeWidget v-if="nodeSuspension" v-bind="nodeSuspension" />
    <NodeWidget v-if="nodePattern" v-bind="nodePattern" />
    <span class="token"> : </span>
    <NodeWidget v-bind="nodeType" />
    <span class="token"> = </span>
    <NodeWidget v-bind="nodeDefault" />
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
