<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import {
  rewritePortValueUpdate,
  WidgetInput,
  type UpdateHandler,
  type WidgetUpdate,
} from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import { EnsoExpression } from '@/components/GraphEditor/widgets/WidgetEnsoExpression.vue'
import {
  createDefaultExpressionOfKind,
  getArgumentDefaultKind,
  type ArgumentDefaultKind,
} from '@/components/GraphEditor/widgets/WidgetFunctionDef/argumentAst'
import SelectionSubmenu from '@/components/GraphEditor/widgets/WidgetSelection/SelectionSubmenu.vue'
import { EnsoTypeExpression } from '@/components/GraphEditor/widgets/WidgetTypeExpression.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { type DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { syntheticPortId, type PortId } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { mapOrUndefined, type Opt } from '@/util/data/opt'
import { proxyRefs } from '@/util/reactivity'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { computed, useTemplateRef } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'
import type { ArgumentDefinition, ConcreteRefs } from 'ydoc-shared/ast'
import WidgetTreeRoot from '../../WidgetTreeRoot.vue'

const { definition, updateCallback, portIdBase, preprocessName } = defineProps<{
  root: Opt<HTMLElement>
  definition: ArgumentDefinition<ConcreteRefs>
  updateCallback: UpdateHandler
  portIdBase: PortId
  preprocessName: (input: string) => string
}>()

const emit = defineEmits<{
  rename: [value: Ast.Owned<Ast.MutableExpression>]
  updateType: [value: Ast.Owned<Ast.MutableExpression>]
  updateDefault: [value: Ast.Owned<Ast.MutableExpression> | undefined]
}>()
type TreeProps = ComponentProps<typeof WidgetTreeRoot>
const openedProject = useCurrentProject()

function defaultWidget(ast: Ast.Token | Ast.Ast): TreeProps {
  return { input: WidgetInput.FromAst(ast), updateCallback }
}

function patternWidget(pattern: Ast.Expression): TreeProps {
  return {
    input: {
      portId: pattern.id,
      value: pattern,
      [EnsoExpression]: { preprocess: preprocessName },
    },
    updateCallback(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, updateCallback, pattern.id, (value) => {
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
  toProps: (ast: T) => TreeProps = defaultWidget,
) {
  return computed(() => mapOrUndefined(child()?.node, toProps))
}

const nodeSuspension = mkWidget(() => definition.suspension)
const nodePattern = mkWidget(() => definition.pattern, patternWidget)
const nodeType = computed((): TreeProps => {
  const ty = definition.type?.type?.node
  const syntheticId = syntheticPortId(portIdBase, 'type')
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(ty, () => syntheticId),
      [EnsoTypeExpression]: {},
    },
    updateCallback(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, updateCallback, syntheticId, (rawValue) => {
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
  const matchingTypeEntry = openedProject.suggestionDb.value.entries.selectableTypes.value.find(
    (ty) => ty.name === tyCode,
  )
  return matchingTypeEntry ?
      openedProject.projectNames.value.printProjectPath(matchingTypeEntry.definitionPath)
    : undefined
}

const nodeDefaultPortId = computed(() => syntheticPortId(portIdBase, 'defaultExpr'))
const nodeDefault = computed((): TreeProps | undefined => {
  if (defaultKind.value !== 'explicit') return

  let expr = Ast.unwrapGroups(definition.defaultValue?.expression?.node)
  if (expr instanceof Ast.Group || expr instanceof Ast.Invalid) expr = undefined
  const syntheticId = nodeDefaultPortId.value
  const expectedType = mapOrUndefined(definition.type?.type?.node, resolveType)
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(expr, () => syntheticId),
      expectedType,
      [EnsoExpression]: {
        weakMatch: true,
      },
    },
    updateCallback(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, updateCallback, syntheticId, (rawValue) => {
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

const submenuRef = useTemplateRef('submenuRef')
const defaultValueRoot = useTemplateRef<HTMLElement>('defaultValueRoot')

// Close the dropdown when clicking outside of it, but also end parent interaction when clicking outside of both.
const defaultValueDropdownInteraction = WidgetEditHandler.NewNested(
  nodeDefaultPortId,
  () => undefined,
  {
    pointerdown: (ev) => {
      if (submenuRef.value?.isTargetOutside(ev)) defaultValueDropdownInteraction.value.end()
    },
  },
)

const defaultKind = computed(() => getArgumentDefaultKind(definition))
const defaultKindText = computed(
  (): string =>
    ({
      required: 'required',
      optional: 'optional',
      explicit: 'default',
    })[defaultKind.value],
)

function defaultOnClick(entry: (typeof defaultEntries)[number]) {
  if (entry.value !== defaultKind.value) {
    emit('updateDefault', createDefaultExpressionOfKind(entry.key, definition.pattern.node.code()))
  }
  defaultValueDropdownInteraction.value.end()
}

function mkDefaultEntry(key: ArgumentDefaultKind, value: string) {
  return proxyRefs({
    value,
    key,
    selected: computed(() => defaultKind.value === key),
  })
}

const defaultEntries = [
  mkDefaultEntry('optional', 'optional argument'),
  mkDefaultEntry('required', 'required argument'),
  mkDefaultEntry('explicit', 'default value'),
] as const satisfies DropdownEntry[]
</script>

<template>
  <div class="ArgumentRow">
    <WidgetTreeRoot v-if="nodeSuspension" v-bind="nodeSuspension" />
    <WidgetTreeRoot v-if="nodePattern" v-bind="nodePattern" />
    <span class="tokenText">&nbsp;:&nbsp;</span>
    <WidgetTreeRoot v-bind="nodeType" />
    <span class="tokenText">&nbsp;=&nbsp;</span>
    <div
      ref="defaultValueRoot"
      class="defaultValueRoot clickable"
      @click.stop="defaultValueDropdownInteraction.start()"
    >
      <SvgIcon
        name="arrow_right_head_only"
        class="dropdownArrow widgetOutOfLayout"
        :class="{ hovered: false }"
      />
      <SelectionSubmenu
        ref="submenuRef"
        :rootElement="root"
        :floatReference="defaultValueRoot"
        :show="defaultValueDropdownInteraction.isActive()"
        :entries="defaultEntries"
        :topLevel="true"
        @clickedEntry="defaultOnClick"
      />
      <span class="tokenText" data-testid="missing-behaviour">{{ defaultKindText }}</span>
    </div>
    <WidgetTreeRoot
      v-if="nodeDefault"
      v-bind="nodeDefault"
      class="pad-left"
      data-testid="missing-default-value"
    />
  </div>
</template>

<style scoped>
.ArgumentRow,
.defaultValueRoot {
  display: flex;
  flex-direction: row;
  place-items: center;
  overflow-x: clip;
}

.pad-left {
  margin-left: 4px;
}

.defaultValueRoot {
  position: relative;
}

svg.dropdownArrow {
  position: absolute;
  bottom: -10px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
  &.hovered {
    opacity: 0.9;
  }
}

.tokenText {
  opacity: 0.33;
}
</style>
